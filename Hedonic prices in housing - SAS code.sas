libname project "C:\Users\victo\Documents\Cours\Mag2\SAS\Project";
PROC IMPORT DATAFILE = "C:\Users\victo\Documents\Cours\Mag2\SAS\Project\Melbourne.csv"
	DBMS=CSV
	OUT=WORK.IMPORT;
	GETNAMES=YES;
RUN;
/* renommage des variables */
DATA Data; set work.import;
rename BuildingArea = surface Yearbuilt = annee Car = parking CouncilArea = syndicat 
Rooms = pieces price = prix distance = CBD bathroom = SDB landsize = terrain
Regionname = region propertycount = voisins Bedroom2 = chambres Suburb = quartier 
address = addresse type = type method = methode sellerg = agent ;
/* nettoyage de la base (valeurs manquantes et aberrantes)*/
DATA Data; set Data(where = (annee>1879));
DATA Data; set Data(where = (300>surface>9));
DATA Data; set Data(where = (chambres<8));
DATA Data; set Data(where = (SDB<7));
DATA melb; set Data(where = (parking<8));
if compress(cats(of surface), '.') = ' ' then delete;
if compress(cats(of annee), '.') = ' ' then delete;
if compress(cats(of parking), '.') = ' ' then delete;
if compress(cats(of syndicat), '.') = ' ' then delete;
if compress(cats(of region), '.') = 'Eastern Victoria' then delete;
if compress(cats(of region), '.') = 'Northern Victoria' then delete;
if compress(cats(of region), '.') = 'Western Victoria' then delete;
/*Creation de variables*/
DATA melb; set melb;
if prix = 0 then logprix = 0; else logprix = log(prix);
logsurface = log(surface); 
if terrain = 0 then logterrain = 0; else logterrain = log(terrain);
if CBD = 0 then logcbd = 0; else logcbd = log(CBD);
logage = log(2020-annee);
house = (Type in ("h"));
RUN;
Proc PRINT Data= melb (OBS =10);
RUN; 
Proc CORR Data= melb;
var prix pieces CBD chambres SDB parking terrain surface logage; 
RUN; 

/******************************************************************************************/ 
/*************** Modele initial, Multicolinearite et variable a�omettre*********************************/ 
/******************************************************************************************/ 
PROC REG DATA=melb plots=NONE; 
model logprix = pieces chambres SDB parking logsurface logcbd logterrain house logage/ tol vif ;
title "Multicolinearite";
output out=melb residual=u_hat predicted=y_hat;
RUN; 
PROC REG DATA=melb plots=NONE; 
model logprix = pieces SDB parking logsurface logcbd logterrain house logage/ tol vif;
title "Retrait de Bedroom2";
output out=melb residual=uhat predicted=yhat;
RUN; 
/******************************************************************************************************************************************/
/********************************* Hétéroscédasticité sous H4 *************************************************************************************/
/******************************************************************************************************************************************/ 
PROC REG DATA=melb plots=NONE; 
model logprix = pieces SDB parking logsurface logcbd logterrain house logage/SPEC;
title "Tests d'heteroscedasticite";
output out=melb residual=u_hat predicted=y_hat;
RUN; 
/************************************************* methode weighted **********************************************************************/ 
data melb; set melb;
resid2 = u_hat*u_hat; /*Apres avoir recupereles residus, on calcule leur carres*/
log_u2 = log(resid2); /*on utilise toujours la forme log*/
run;
/*On estime le carre des residus*/ 
proc reg data = melb outest = stats  rsquare plots = none;
model log_u2 = pieces SDB parking logsurface logcbd logterrain house logage;
output out = pred_u2 p = pred_log_u2;
title "estimation de la variance OLS";
run; 
/*On calcul 1/sigma2*/
data melb; set pred_u2; run;
data melb; set pred_u2;
sigma2 = exp(pred_log_u2); 
one_over_sigma = 1/(sigma2**0.5);  
run;
/*Et on l'utilise dans une regression WLS*/ 
proc model data = melb plots = none; 
logprix=b0+b_pieces*pieces+b_sdb*sdb+b_parking*parking+b_logsurface*logsurface+b_logcbd*logcbd+b_logterrain*logterrain+b_house*house+b_logage*logage;  
fit logprix;
weight one_over_sigma;
title "Modele WLS";
run; 
/************************************************* HCCME **********************************************************************/
proc model data = melb plots = none; 
logprix=b0+b_pieces*pieces+b_sdb*sdb+b_parking*parking+b_logsurface*logsurface+b_logcbd*logcbd+b_logterrain*logterrain+b_house*house+b_logage*logage;   
fit logprix / HCCME=1;
title "Correction HCCME";
run;

/******************************************************************************************/ 
/***************************** Endogeneite ************************************************/ 
/******************************************************************************************/ 
/*on test plusieurs instruments possibles+ White test*/ 

DATA melb; set melb;
lnCount = log(voisins);
RUN;


PROC PRINT DATA=melb (OBS=30); 
RUN; 
proc model data = melb plots = none; 
logprix=b0+b_pieces*pieces+b_sdb*sdb+b_parking*parking+b_logsurface*logsurface+b_logcbd*logcbd+b_logterrain*logterrain+b_house*house+b_logage*logage;
exogenous logterrain pieces SDB parking logcbd logage house;
endogenous logprix logsurface;
instruments _exog_  Postcode lnCount;   
fit logprix / ols out=residuals outresid;
title "Endogeneite";
run; 
proc model data = melb plots = none; 
logprix=b0+b_pieces*pieces+b_sdb*sdb+b_parking*parking+b_logsurface*logsurface+b_logcbd*logcbd+b_logterrain*logterrain+b_house*house+b_logage*logage;
exogenous logterrain pieces SDB parking logcbd logage house ; 
endogenous logprix logsurface;
instruments _exog_ Postcode lnCount ;  
fit logprix / 2SLS out=iv_residuals outresid WHITE ; 
title "Endogeneite par 2SLS";
run; 
/*On rejette l'hypothese nulle, les resultats sont heteroscedastiques
On applique le test de Hansen pour HZ1*/ 

proc model data = melb plots = none; 
logprix=b0+b_pieces*pieces+b_sdb*sdb+b_parking*parking+b_logsurface*logsurface+b_logcbd*logcbd+b_logterrain*logterrain+b_house*house+b_logage*logage;
exogenous logterrain pieces SDB parking logcbd logage house;
endogenous logprix logsurface;
instruments _exog_ Postcode lnCount;  
fit logprix / GMM KERNEL = (PARZEN,0,) HCCME=1 WHITE;
title "Test de Hansen, avec GMM et HCCME";
run; 
/********************************************************************* Weak instrument ***************************************************/ 
proc reg data = melb plots = none;
model logprix = pieces SDB parking logsurface logcbd logterrain house logage Postcode lnCount;
output out = logsurface_resid r = logsurface_residuals; 
test Postcode=0, lnCount=0; 
run;

/* Test de Hausman */ 
proc model data = melb plots = none; 
logprix=b0+b_pieces*pieces+b_sdb*sdb+b_parking*parking+b_logsurface*logsurface+b_logcbd*logcbd+b_logterrain*logterrain+b_house*house+b_logage*logage;
exogenous logterrain pieces SDB parking logcbd logage house;
endogenous logprix logsurface;
instruments _exog_ Postcode lnCount;
fit logprix / OLS 2SLS HAUSMAN HCCME =1; 
title "Endogeneite & test d'Hausman ";
run;
