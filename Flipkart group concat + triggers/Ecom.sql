
USE FLIPKART;
SELECT * FROM region_date_delivered;

SET SQL_SAFE_UPDATES = 0;
/*Converting the DateofRSPS from string to date and then to the desired date format*/
UPDATE region_date_delivered
SET DateofRSPS = date_format(str_to_date(DateOfRSPS,'%d-%b-%Y'),'%d-%b-%Y');
/*Pivot table using case when and view */
Create view Ecom_region as (
 SELECT `Region-zone` as Regions , `Hub Name` , FHRID, `DeliveryType`,
coalesce(sum((case when DateOfRSPS = "01-May-2020" then delivered end)), ' ') as "01-May-2020",
    coalesce(sum((case when DateOfRSPS = "02-May-2020" then delivered end)), ' ') as "02-May-2020",
    coalesce(sum((case when DateOfRSPS = "03-May-2020" then delivered end)), ' ') as "03-May-2020",
    coalesce(sum((case when DateOfRSPS = "04-May-2020" then delivered end)), ' ') as "04-May-2020",
    coalesce(sum((case when DateOfRSPS = "05-May-2020" then delivered end)), ' ') as "05-May-2020"
  from region_date_delivered
  Group by FHRID
  Order by `Hub Name`,FHRID, delivered desc );
  Select * from Ecom_region;
   Select 
     Regions , `Hub Name` , FHRID, `DeliveryType`,`01-May-2020`,`02-May-2020`,`03-May-2020`,`04-May-2020`,`05-May-2020`,
     (`01-May-2020`+`02-May-2020`+`03-May-2020`+`04-May-2020`+`05-May-2020`) as GrandTotal
     FROM Ecom_region
UNION
SELECT 'GrandTotal','','','',sum(`01-May-2020`),sum(`02-May-2020`),sum(`03-May-2020`),sum(`04-May-2020`),sum(`05-May-2020`),
sum(`01-May-2020`)+sum(`02-May-2020`)+sum(`03-May-2020`)+sum(`04-May-2020`)+sum(`05-May-2020`) 
FROM Ecom_region;

/*Pivot table using group_concat*/
/* Objective: To calculate the number of items delivered from May 1,2020 to May 5,2020 with 
respect to regions, HubName, FHRID, DeliveryType */
/* Step 1: Initialize the variable */
Set @r1:=0 ;
/* Step 2: Here we will do the actual pivot operation using the column (here DateofRSPS) which we actually want to transpose to column
group concat the sum-if query to transpose the column 'DateofRSPS' in single quotes using distinct concat so that only the unique values of 
 DateofRSPS get concatenated with the query and ordering them in ascending order of month. Store the concatenated query into r1 variable initialized above
 Using ifnull to remove all the null values from the pivot table*/
SELECT group_concat( distinct concat( 'ifnull(sum(if(DateOfRSPS="',DateOfRSPS,'" ,Delivered,Null))," ") as "',DateOfRSPS,'"' ) 
order by DateOfRSPS )  into @r1  from region_date_delivered ;
/*Showing the output of the above query*/
SELECT @r1;
/* Step 3: Here we will concatenate the rest of the columns along with r1 variable.
Here we want to display the items 'Delivered' across 5 days of DateofRSPS (the column which has been transposed) with respect to Regions, HubName,FHRID,
DeliveryType and concat the Grand total column in a single query. */
SET @r2 := concat('Select `Region-zone` , `Hub Name` , FHRID, DeliveryType, ',@r1, ', sum(if(DateOfRSPS>="01-May-2020" and DateOfRSPS<="05-May-2020",Delivered, Null)) as GrandTotal from region_date_delivered Group by FHRID Order by `Hub Name`,FHRID, delivered desc  ') ;
/* Showing the output of the above query */
SELECT @r2;
/* Step 4: Preparing the statement from r2 variable above */
Prepare sd1 from @r2 ;
/* Step 5: Execute the statement above*/
Execute  sd1 ;


