CREATE DATABASE FLIPKART;
USE FLIPKART;
SELECT * FROM region_date_delivered;
SELECT * FROM flipkart_hubs;

SET SQL_SAFE_UPDATES = 0;

/* Conversion of date from 19-Apr-20 to 2020-04-19 */
UPDATE flipkart_hubs
SET Date = str_to_date(Date,'%d-%b-%y');

/*Q-3*/
SELECT WeekNum , count(*) as `Count of Hubs`
FROM flipkart_hubs
Group by WeekNum;

/*Steps for converting rows to columns. Reference from : https://stackoverflow.com/questions/1241178/mysql-rows-to-columns/9668036#9668036?newreg=93f5a8e292624aeaa92cbfe1b06560ae */
/*Step 1: extend the base table with extra columns*/
Create view Region_extended as (
  select
    region_date_delivered.*,
    case when DateOfRSPS = "01-May-20" then delivered end as "01-May-20",
    case when DateOfRSPS = "02-May-20" then delivered end as "02-May-20",
    case when DateOfRSPS = "03-May-20" then delivered end as "03-May-20",
    case when DateOfRSPS = "04-May-20" then delivered end as "04-May-20",
    case when DateOfRSPS = "05-May-20" then delivered end as "05-May-20"
  from region_date_delivered
);
SELECT * FROM Region_extended;

/*Step 2: group and aggregate the extended table. */
Create view Region_extended_pivot as (
SELECT `Region-zone` as Regions , `Hub Name` , FHRID, `DeliveryType`,
            sum(`01-May-20`) as `01-May-20`,
            sum(`02-May-20`) as `02-May-20`,
            sum(`03-May-20`) as `03-May-20`,
            sum(`04-May-20`) as `04-May-20`,
            sum(`05-May-20`) as `05-May-20`
            from Region_extended
            Group by FHRID
            );
  SELECT * FROM Region_extended_pivot;         
   Drop view Region_extended_pivot;

/* Step 3 : replace any null values with zeroes so the result set is nicer to look at */
create view Region_extended_pretty as (
  select 
     Regions , `Hub Name` , FHRID, `DeliveryType`,
    coalesce(`01-May-20`, ' ') as `01-May-20`, 
    coalesce(`02-May-20`, ' ') as `02-May-20`, 
    coalesce(`03-May-20`, ' ') as `03-May-20`,
    coalesce(`04-May-20`, ' ') as `04-May-20`, 
    coalesce(`05-May-20`, ' ') as `05-May-20`
  from Region_extended_pivot
);

select * from Region_extended_pretty;
Drop view Region_extended_pretty;

  SELECT * FROM flipkart_region;
 Drop view flipkart_region;
 
 /*Q-4*/
 Create view flipkart_region as (
 SELECT `Region-zone` as Regions , `Hub Name` , FHRID, `DeliveryType`,
coalesce(sum((case when DateOfRSPS = "01-May-2020" then delivered end)), ' ') as "01-May-2020",
    coalesce(sum((case when DateOfRSPS = "02-May-2020" then delivered end)), ' ') as "02-May-2020",
    coalesce(sum((case when DateOfRSPS = "03-May-2020" then delivered end)), ' ') as "03-May-2020",
    coalesce(sum((case when DateOfRSPS = "04-May-2020" then delivered end)), ' ') as "04-May-2020",
    coalesce(sum((case when DateOfRSPS = "05-May-2020" then delivered end)), ' ') as "05-May-2020"
  from region_date_delivered
  Group by FHRID
  Order by `Hub Name`,FHRID, delivered desc );
  
   Select 
     Regions , `Hub Name` , FHRID, `DeliveryType`,`01-May-20`,`02-May-20`,`03-May-20`,`04-May-20`,`05-May-20`,
     (`01-May-20`+`02-May-20`+`03-May-20`+`04-May-20`+`05-May-20`) as GrandTotal
     FROM flipkart_region;
  
  Select 
     Regions , `Hub Name` , FHRID, `DeliveryType`,`01-May-20`,`02-May-20`,`03-May-20`,`04-May-20`,`05-May-20`,
     (`01-May-20`+`02-May-20`+`03-May-20`+`04-May-20`+`05-May-20`) as GrandTotal
     FROM flipkart_region
UNION
SELECT 'GrandTotal','','','',sum(`01-May-20`),sum(`02-May-20`),sum(`03-May-20`),sum(`04-May-20`),sum(`05-May-20`),
sum(`01-May-20`)+sum(`02-May-20`)+sum(`03-May-20`)+sum(`04-May-20`)+sum(`05-May-20`) 
FROM flipkart_region;

set @r1:=0 ;

select group_concat( distinct concat( 'ifnull(sum(if(DateOfRSPS="',DateOfRSPS,'" ,Delivered,Null))," ") as "',DateOfRSPS,'"' ) order by DateOfRSPS )  into @r1  from region_date_delivered ;

set @r2 := concat('Select `Region-zone` , `Hub Name` , FHRID, DeliveryType, ',@r1, '  from region_date_delivered Group by FHRID Order by `Hub Name`,FHRID, delivered desc  ') ;
Prepare sd1 from @r2 ;
execute  sd1 ;
select @r1;

UPDATE region_date_delivered
SET DateofRSPS = date_format(str_to_date(DateOfRSPS,'%d-%b-%Y'),'%d-%b-%Y');
