/* ----------------------------------------------------------------------------
 * | PART 2
 * ----------------------------------------------------------------------------
 * In this exercise we will consider the neurological response (IMP) of the 
 * child at time points 10 and 18 months. Create a data set where the IMP 
 * scores at 10 and 18 months are in different columns next to a column that 
 * indicates the child identity (ID).
 */

LIBNAME SASDATA '/folders/myfolders/sample_exam/';
PROC SQL;
	CREATE TABLE DATA AS
	SELECT A.ID as ID, A.IMP as IMP_10, B.IMP as IMP_18 
	FROM SASDATA.ivf AS A, SASDATA.ivf AS B 
	WHERE A.ID = B.ID and A.PER = 10 AND B.PER = 18;
RUN;

/* ----------------------------------------------------------------------------
 * | QUESTION 2A
 * ----------------------------------------------------------------------------
 * Apply Grubbs outlier test on the neurological response at 10 months to 
 * determine if there exist an extreme lower neurological response. Report the 
 * test statistic, the critical value, and discuss the results.
 */

/**
 * alpha: Significance level (default = 0.05)
 * num: Number of observations in dataset
 * ds: Input dataset
 */
%macro grubbs_crit(alpha=0.05, num=, ds=);
	data &ds;
		t2=tinv(&alpha/(2*&num), &num - 2);
		gcrit2=((&num-1)/sqrt(&num))*sqrt(t2*t2/(&num-2+t2*t2));
		label gcrit2='Critical (95%) Two-sided Grubbs Multiplier';
	run;
%mend grubbs_crit;

%macro means(var=, in=, outp=);
	proc means data=&in;
		var &var;
		output out=&outp mean=mean std=std n=n var=var;
	run;
%mend means;

%macro grubbs(n=, inp=, outp=, var=, mean=, variance=, grubbs_crit=);
	DATA &outp;
		SET &inp;
		N = &n;
		U = (&var-&mean)/sqrt(&variance);
		Grubbs=ABS(U);
		C_twosided= &grubbs_crit;
	RUN;
%mend;

%macro doornbos(n=, inp=, outp=, var=, mean=, variance=);
	DATA &outp;
		SET &inp;
		N=&n;
		U = (&var-&mean)/SQRT(&variance);
		W = SQRT((N*(N-2)*U**2)/((N-1)**2-N*U**2));
		DOORNBOS_Y=ABS(W);
		CRITER= QUANTILE('T', 1-0.05/(2*N), N-2);
	RUN;
%mend;


%means(var=IMP_10, in=data, outp=means_10);
%means(var=IMP_18, in=data, outp=means_18);
%grubbs_crit(num=240, ds=grubbs_crit);

%grubbs(n=240, inp=data, outp=grubbs_10, var=IMP_10, mean=90.4041667, variance=10.24182357, grubbs_crit=3.6594939273);
%doornbos(n=240, inp=data, outp=doornbos_10, var=IMP_10, mean=90.4041667, variance=10.24182357);

%grubbs(n=240, inp=data, outp=grubbs_18, var=IMP_18, mean=91.087866109, variance=6.7023311417, grubbs_crit=3.6594939273);
%doornbos(n=240, inp=data, outp=doornbos_18, var=IMP_18, mean=91.087866109, variance=6.7023311417);

/* ----------------------------------------------------------------------------
 * | QUESTION 2B
 * ----------------------------------------------------------------------------
 * Calculate Pearson's rho correlation coecient between the neurological 
 * response at 10 and 18 months and provide an appropriate 95% confidence 
 * interval on this correlation coecient.
 */
proc corr data=data FISHER;
	var imp_10 imp_18;
run;

/* ----------------------------------------------------------------------------
 * | QUESTION 2C
 * ----------------------------------------------------------------------------
 * It is assumed that the Farlie-Gumbel-Morgenstern copula describes the 
 * association between the neurological responses at 10 and 18 months. Estimate
 * the association parameter.
 */
PROC CORR DATA=DATA KENDALL;
	VAR IMP_10;
	WITH IMP_18;
RUN;

/* 
 * In case a specific copula function is assumed, the association parameter 
 * can be estimated with τ:
 * - Gaussian: ρ = sin π*τ/2
 * - Gumbel: α = 1/ 1 − τ
 * - Clayton: α = 2/ 1 − τ
 * - FGM: α = 9 τ/2 
 * 
 * If the result is not in the range of -1, 1, then it is unlikely to be the
 * underlying copula.
 * 
 * In this case, we have 9*0.17265/2 = 0.776925
 */

/* ----------------------------------------------------------------------------
 * | QUESTION 2D
 * ----------------------------------------------------------------------------
 * From the age of 10 months it is expected that the neurological score is at 
 * least 85. In case it is lower, the child is at risk to develop a 
 * neurological deciency. Test the null hypothesis that the proportion of 
 * children with a possible neurological deciency is the same at 10 and 18 
 * months. Report the null hypothesis, the alternative hypothesis, the test 
 * statistic, the result of the test statistic, the p-value, and the 
 * conclusion.
 */

/*
 * H0: prop_10 - prop_18 = 0
 * H1: prop_10 - prop_18 > 0
 */

DATA PROP;
	SET SASDATA.ivf;
	WHERE PER = 10 OR PER = 18;
	PROP = IMP < 85;
RUN;

PROC SORT DATA=PROP;
	BY PER;
RUN;

PROC FREQ DATA=PROP;
	TABLES PER*PROP /NOROW NOCOL EXPECTED  CHISQ;
	ODS OUTPUT FishersExact=Fishers ChiSq=ChiSq;
RUN;
/* Fisher (0.1373) and Chi-squared (0.0994) could not reject null hypothesis */



DATA PROP;
	SET DATA;
	PROP_10 = (IMP_10<85);
	PROP_18 = (IMP_18<85);
RUN;

PROC MEANS DATA=PROP;
	VAR PROP_10 PROP_18;
RUN;

/* Manually:
 * alpha = 0.05 --> Z-VALUE: 1.65
 * std = PROP_10_STD - PROP_18_STD = 0.2990039 - 0.2366321 = 0.0623718
 * mean = PROP_10_MEAN - PROP_18_MEAN = 0.0988142 - 0.0592885 = 0.0395257
 * IF MEAN > STD * Z-VALUE, we can reject null hypothesis
 * That is not the case.
 */

/*
 * Calculate Z-VALUE with confidence level
 */
DATA Z;
	Z_VALUE = QUANTILE('NORMAL', 0.95);
run;

