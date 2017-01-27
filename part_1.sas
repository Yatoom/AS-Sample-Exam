/* ----------------------------------------------------------------------------
 * | PART 1
 * ----------------------------------------------------------------------------
 * In this exercise we will consider gestational age ( GA ) as the outcome 
 * variable of interest and ignore all other variables. To be able to answer 
 * the questions, create a data set that contains only data from period 4 
 * months (PER=4) and calculate the transformation log (44 − GA), with log the 
 * natural logarithm.
 */

/* Macros */
%macro ci_mean(data, column, quant);
	proc means data=&data noprint;
		var &column;
		output out=summary mean=mean std=std n=n;
	run;
	data ci_mean;
		set summary;
		t_value = quantile('T', 1.0-(1.0-&quant)/2, n/2 - 1);
		lower = mean - std * t_value / sqrt(n);
		upper = mean + std * t_value / sqrt(n);
		drop _TYPE_ _FREQ_;
	run;
	title 'Confidence interval';
	proc print data=ci_mean;
	run;
	
	Title;
		
%mend ci_mean;

%macro pi_mean(data, column, quant);
	proc means data=&data noprint;
		var &column;
		output out=summary mean=mean std=std n=n;
	run;
	data pi_mean;
		set summary;
		t_value = quantile('T', 1.0-(1.0-&quant)/2, n/2 - 1);
		lower = mean - t_value * sqrt((n+1) * STD**2 / n);
		upper = mean + t_value * sqrt((n+1) * STD**2 / n);
		drop _TYPE_ _FREQ_;
	run;
	title 'Prediction interval';
	proc print data=pi_mean;
	run;
	
	title '';	
%mend pi_mean;


/* There are three ways to get data
 * - Enter data manually in the CODE window (Data step)
 * - Import stored data in an external format (PROC IMPORT)
 * - Import stored data in SAS format (LIBNAME)
 */

LIBNAME SASDATA '/folders/myfolders/sample_exam/';
DATA DATA;
	SET SASDATA.ivf;
	WHERE PER=4;
	GA_TRANSFORMED = log(44 - GA);
RUN;

/* ----------------------------------------------------------------------------
 * | QUESTION 1A
 * ----------------------------------------------------------------------------
 * Report the first, second, and third quartile, the IQR, the percentiles
 * p1, p5, p95 and p99, the minimum, the maximum, and the range for 
 * gestational age (GA).
 */

proc univariate data=DATA;
	VAR GA_TRANSFORMED;
RUN;

/* 4 quartile = 1 quantile = 100 percentile
 * IQR: Interquartile range
 */
 
/* ----------------------------------------------------------------------------
 * | QUESTION 1B
 * ----------------------------------------------------------------------------
 * Provide a 95% confidence interval on the first quartile for gestational age.
 * Provide the necessary calculation details.
 */

proc univariate data=DATA ciquantdf;
	VAR GA_TRANSFORMED;
RUN;

/* ----------------------------------------------------------------------------
 * | QUESTION 1C
 * ----------------------------------------------------------------------------
 * Test if gestational age is normally distributed. Report the value of the 
 * test statistic, the p -value, the conclusion, and the reason for choosing
 * this specic test statistic.
 */

/* Tests for normality:
 * 
 * Chi-square test
 * - Useful for grouped data and always applicable
 * - Not recommended when distributional parameters should be estimated and 
 *   numerical data is present
 * - Not recommended as omnibus test
 * 
 * Shapiro-Wilk
 * - A very sensitive and powerful test against nonsymmetric departures from 
 *   normality.
 * - Probably the most powerful test of all others and thus recommended as 
 *   omnibus test
 * - Sensitive to ties in the data: when ties are a result of rounding, 
 *   then normality can incorrectly be rejected. Data has tie(s) when two or 
 *   more observations have an identical value
 * 
 * Anderson-Darling
 * - Has often the same power as the Shapiro-Wilk, but is overall somewhat less
 * - Is less sensitive to ties
 * - Recommended as omnibus test
 * 
 * D’Agostino test
 * - Is generally outperformed by other tests
 * - Is not recommended as omnibus test
 * - Is most powerful for symmetric platykurtic alternatives
 */


proc univariate data=DATA normal;
	var GA_TRANSFORMED;
	qqplot / normal;
run;

/* ----------------------------------------------------------------------------
 * | QUESTION 1D
 * ----------------------------------------------------------------------------
 * Using the normality assumption of the transformation log (44 - GA), give a 
 * 95% prediction interval for a new observation of gestational age.
 */

/*
 * # Confidence intervals (CI)
 * An interval for population characteristic with a predefined confidence level
 * 
 * # Prediction intervals (PI)
 * An interval for a new random draw with a predefined confidence level
 * 
 * # Tolerance intervals (TI)
 * An interval for a portion of the population with a predefined confidence 
 * level
 */

%ci_mean(data, ga_transformed, 0.95);
%pi_mean(data, ga_transformed, 0.95);

/* ----------------------------------------------------------------------------
 * | QUESTION 1E
 * ----------------------------------------------------------------------------
 * Premature birth is defined as gestational age less than 37 weeks. Literature 
 * suggests that premature births occur once in every ten deliveries. Report 
 * the proportion of premature births with an appropriate 95% condence 
 * interval. Conclude if the group of women under study have a higher risk of
 * premature births?
 */

DATA PROP;
	SET DATA;
	PROP = (GA<37);
RUN;

PROC MEANS DATA=PROP noprint;
	VAR PROP;
	OUTPUT OUT=SUMMARY MEAN=MEAN N=N;
RUN;

title 'CI Proportions';

DATA CI_PROP;
	SET SUMMARY;
	QUANT = 0.95;
	
	Z_VALUE = QUANTILE('NORMAL', 1-(1-QUANT)/2);
	VAR  = MEAN*(1-MEAN);
	LCL = MEAN - Z_VALUE * SQRT(VAR/N);
	UCL = MEAN + Z_VALUE * SQRT(VAR/N);
	DROP _TYPE_ _FREQ_;
RUN;

PROC PRINT DATA=CI_PROP;
RUN;

