libname t "D:\FAA Competition\Cluster\Vihang Patil\cluster";
proc contents data=t.cars varnum;
run;
/*missing values*/
proc means data=t.cars nmiss;
run;

/*outliers distribution*/
proc means data=t.cars nmiss p1 p5 p10 p90 p99 max;
run;
/*correlation */
proc corr data=t.cars;
var
Car_no
MPG
Cylinders
Displacement
Horsepower
Weight
Acceleration;
run;
/*without displacement*/
proc corr data=t.cars;
var
Car_no
MPG
Cylinders

Horsepower
Weight
Acceleration;
run;

/*Hierarchial clustering*/
/* proc cluster allows standardisation on the go */
/* The output of this file contains sprsq, which we cna plot to decide the number of clusters. Elbow method can be used here */
/* plot it using excel */
proc cluster 
data=t.cars
standard /* automated standardisation of variables */

method= ward /* how to combine clusters */
outtree=t.tree;/* dendogram */
var
MPG
Cylinders
Horsepower
Weight
Acceleration;
id Car_no;/* row identifier*/
run;



/*interpreting the data */
 proc tree data=t.tree nclusters=3 out=t.hclus_results;
 run;

/*converting from text to numeric */

data t.hclus_results;
set t.hclus_results;
Car_no=_name_*1;
run;


/* creating a combined table */
proc sql;
create table t.hclus_all as
select a.*,b.cluster
from
t.cars as a 
left join
t.hclus_results as b
on
a.car_no=b.car_no
Order by cluster;
quit;

/*profiling the clusters*/

proc means data=t.hclus_all mean std min max;
var
Car_no
MPG
Cylinders
Displacement
Horsepower
Weight
Acceleration;
by cluster;
run;

/* now we use the centroid centers in kmeans */

/* we need to standardise the centroid means */

proc standard data=t.hclus_all out=t.hclus_all_std
mean=0 std=1;
var
MPG
Cylinders
Displacement
Horsepower
Weight
Acceleration;
run;


proc means data=t.hclus_all_std mean std min max;
var
MPG
Cylinders
Displacement
Horsepower
Weight
Acceleration;
by cluster;
run;


/* creating a training(dev) and validation set */


data t.dev t.val;
set t.hclus_all_std;
if ranuni(666)<0.7
then output t.dev;
else output t.val;
run;


/* Kmeans CLustring */
proc fastclus
data=t.dev
maxc=3 /* number of clusters*/
maxiter=100 /* number of iterations of the loop*/
out=t.dev_results
outseed=t.dev_outseed;
var
MPG
Cylinders
Horsepower
Weight
Acceleration;
id Car_no;
run;

/*validation
compare mean,max,min , std dev and also the percent of a particular cluster in validation sample
For the training and validation they must be similar */

/* map result of dev to original */
proc sql;
create table t.dev_all as
select a.*,b.cluster 
from 
t.cars as a
inner join
t.dev_results as b
on 
a.car_no=b.car_no
Order by cluster;
quit;

/*profiling the results */
proc means data=t.dev_all n mean std min max;
var
MPG
Cylinders
Displacement
Horsepower
Weight
Acceleration;
by cluster;
run;
/*running the centroids of kmeans on validation data */

proc fastclus
data=t.val
maxc=3 /* number of clusters*/
seed=t.dev_outseed
maxiter=0 /* number of iterations of the loop will be 0 as we know the centroids*/
out=t.val_results;
var
MPG
Cylinders
Horsepower
Weight
Acceleration;
id Car_no;
run;

/*join val results */
proc sql;
create table t.val_all as
select a.*,b.cluster 
from 
t.cars as a
inner join
t.val_results as b
on 
a.car_no=b.car_no
Order by cluster;
quit;

/*profiling the val results*/
proc means data=t.val_all n mean std min max;
var
MPG
Cylinders
Displacement
Horsepower
Weight
Acceleration;
by cluster;
run;

/* replace the means variables used in Dev_outseed with means of the variables from hierarchial clustering */ 
/* if you are not able to find the sudden change in the sprsqr graph, compare results for range(+3,-3) of clusters*/
/*giving weightages to variables??? */
/*cap the data for outliers */

 
