CREATE DATABASE Taxi_for_sure;
Use Taxi_for_sure;
SELECT * FROM Localities;
SELECT * FROM Taxi_data;

/*Q2. Make a table with count of bookings with booking_type = p2p catgorized by booking mode as 'phone', 'online','app',etc*/

/*Correct one*/
SELECT Booking_mode, Count(*) as CountOfBookings 
FROM Taxi_Data
WHERE booking_type = "p2p"
Group by Booking_mode;

/*Q4. Find top 5 drop zones in terms of average revenue*/
/*Correct one*/
SELECT Drop_Area, Avg(Fare) as Revenue
FROM Taxi_Data
Group By Drop_Area
ORDER BY 2 DESC
Limit 5;

/*Q5. Find all unique driver numbers grouped by top 5 pickzones*/

SELECT zone_id, driver_number
FROM Taxi_Data d INNER JOIN Localities L ON d.pickup_area = L.area
WHERE zone_id IN (SELECT zone_id
FROM Taxi_Data d INNER JOIN Localities L ON d.pickup_area = L.area
Group By zone_id
ORDER BY Sum(Fare) DESC
limit 5) 
Group By Zone_id, driver_number
HAVING Count(*) = 1;

SELECT zone_id, driver_number
FROM Taxi_Data d INNER JOIN Localities L ON d.pickup_area = L.area
WHERE zone_id IN (SELECT * FROM Top5Zone) 
Group By Zone_id, driver_number
HAVING Count(*) = 1
order by zone_id;

CREATE VIEW Top5Zone As
SELECT zone_id
FROM Taxi_Data d INNER JOIN Localities L ON d.pickup_area = L.area
Group By zone_id
ORDER BY Sum(Fare) DESC
limit 5;

Select * from Top5Zone;
/*Q7. Make a hourwise table of bookings for week between Nov01-Nov-07 and highlight the hours with more than average no.of bookings day wise*/

SET SQL_SAFE_UPDATES = 0;

UPDATE Taxi_data
SET pickup_date= str_to_date(pickup_date, "%c/%e/%Y");
/*Combined query*/

Select Hour(Pickup_time), Count(*)
FROM Taxi_data
WHERE pickup_date between '2013-11-01' AND '2013-11-07'
Group By Hour(pickup_time)
HAVING Count(*) >
(Select Avg(Bookings) as AvgDayWiseBookings
From (Select hour(pickup_time), Count(*) as Bookings
FROM Taxi_data
WHERE pickup_date between '2013-11-01' AND '2013-11-07'
Group By hour(pickup_time)) as tt)
Order by Hour(pickup_time);


Select  concat(Hour(pickup_time),':00-',Hour(pickup_time)+1,':00') as Hour,count(*) as  Bookings  
from  Taxi_data  where  pickup_date>='2013/11/01'  and  pickup_date<='2013/11/07'  
group by hour   
Having  bookings>=( select  avg(book) from 
(select hour(pickup_time),count(*) as book   
from Taxi_data where  pickup_date>='2013/11/01'  and  pickup_date<='2013/11/07'  
group by  hour(pickup_time) ) as n1 ) 
order by bookings desc ;

Select Hour(Pickup_time), Count(*)
FROM Taxi_data
WHERE pickup_date>='2013-11-01'  and  pickup_date<='2013-11-07'
Group By Hour(pickup_time)
HAVING Count(*) >(Select Avg(Bookings) as AvgDayWiseBookings
From (Select hour(Pickup_time), Count(*) as Bookings
FROM Taxi_data
WHERE  pickup_date>='2013-11-01'  and  pickup_date<='2013-11-07'
Group By Hour(pickup_time) ) as n1 )
Order by Hour(pickup_time) ;
