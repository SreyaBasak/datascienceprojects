CREATE DATABASE SQL_Internship;
Use SQL_Internship;
SELECT * FROM Drivers;
/*Use the given data to Enlist all the Drivers, earning less than or equal to 20k or earning more than or equal to 50k.*/
SELECT * 
FROM Drivers
WHERE salary <=20000 or salary >=50000
Order by salary;
/*Use the given Restaurants Datasets to find the cities with at least 2 Restaurants with 5 star Ratings.
Your Output table should contain the city name along with number of 5 star restaurants in that city.
Order the records in Descending order of No. of Restaurants and in case of tie , sort them in alphabetical order.*/
SELECT * FROM Restaurants;
SELECT City, count(*) as Count_of_5_star
FROM Restaurants
WHERE stars =5
Group by City
Having count(*)>=2
Order by count(*) desc, City;
/*Use the given House Dataset to find the Top 3 Postal Codes for each year with Average Price of that Postal Code as the Ranking criteria.*/
SELECT * FROM House;
SELECT Postcode, Year, Avg_Price, YearWiseRank FROM
(SELECT Postcode, Year, Avg_Price ,
Rank() Over (Partition by year Order By Avg_Price desc) as YearWiseRank
FROM (SELECT Year, Postcode, Round(avg(price),2) as Avg_price
FROM House
Group by year,Postcode
order by year) as tt) as tt1
WHERE YearWiseRank<=3;
/*Use the given data to enlist the employees who have received raise in their salary for at least 3 consecutive years.*/
SELECT * FROM EMPLOYEE;
Create view rise_in_salary as(
SELECT employee_id ,  
sum(CASE WHEN year = 2016 THEN Salary END ) AS "y2016",
       sum(CASE WHEN year = 2017 THEN Salary END) AS "y2017",
       sum(CASE WHEN year = 2018 THEN Salary END) AS "y2018",
       sum(CASE WHEN year = 2019 THEN Salary END) AS "y2019",
       sum(CASE WHEN year = 2020 THEN Salary END) AS "y2020"
       FROM Employee 
       Group by Employee_id);
       SELECT employee_id FROM rise_in_salary
       where y2016 < y2017 and y2017 < y2018 and y2018 < y2019
       UNION 
       SELECT employee_id FROM rise_in_salary
       where y2017 < y2018 and y2018 < y2019 and y2019 < y2020;
       
SELECT employee_id
from (select *,
             lag(salary) over (partition by employee_id order by year) as prev_salary
      from employee
     ) as tt
group by employee_id
having min(salary - prev_salary) > 0 and
       count(*) = 4;
       
/*In the following dataset 1-2,3-4,5-6,7-8 form a pair but the class teacher decides to interchange the students ids of each pair. 
Help the teacher to write a SQL query to interchange all the pair id in one go.*/
SELECT * FROM `Input Table`;
SELECT IF(cnt % 2 = 1 AND id = cnt, id, IF(id % 2 = 1, id + 1, id - 1)) AS student_id, stu_name FROM `Input Table`,
(SELECT COUNT(*) AS cnt FROM `Input Table`) AS tt
ORDER BY student_id;
SELECT * FROM `Input Table`;
SELECT IF(id % 2 = 1, id + 1, id - 1) AS student_id, stu_name FROM `Input Table`,
(SELECT COUNT(*) AS cnt FROM `Input Table`) AS tt
ORDER BY student_id;
/*You’re given a table of rental property searches by users.

 The table consists of search results and outputs host information for searchers. 
 Find the minimum, average, maximum rental prices for each host’s popularity rating.
 The host’s popularity rating is defined as below:
    0 reviews: New
    1 to 5 reviews: Rising
    6 to 15 reviews: Trending Up
    16 to 40 reviews: Popular
    more than 40 reviews: Hot
Tip: The `id` column in the table refers to the search ID.*/
SELECT * FROM host_details;
SELECT min(price) as Min_Price,Round(Avg(Price),2) as Avg_Price, max(Price) as Max_price, Case
WHEN no_of_reviews =0 Then "New"
WHEN no_of_reviews Between 1 and 5 then "Rising"
WHEN no_of_reviews Between 6 and 15 then "Trending Up"
WHEN no_of_reviews Between 16 and 40 then "Popular"
WHEN no_of_reviews > 40 then "Hot"
Else "Not available"
End as popularity_rating
FROM host_details
Group by popularity_rating
order by No_of_reviews;
SELECT id ,price,propert_type,room_type,zipcode, no_of_reviews, Case
WHEN no_of_reviews =0 Then "New"
WHEN no_of_reviews Between 1 and 5 then "Rising"
WHEN no_of_reviews Between 6 and 15 then "Trending Up"
WHEN no_of_reviews Between 16 and 40 then "Popular"
Else "Hot"
End as Status
FROM host_details;
/*Use the given data to find the customer with the highest total order cost within a single day  between 
2021-02-01 to 2021-03-01.Total order cost is calculated as order_cost*order_quantity. 
Output their first name, total sales amount and the date. 
For simplicity, you can assume that every first name in the dataset is unique. Also, the cost of the certain item 
(e.g. shoes) could vary among different purchases (not all shoes cost the same)*/
SELECT * FROM Customers;
SELECT * FROM Orders;
SET SQL_SAFE_UPDATES = 0;
UPDATE Orders
SET order_date= str_to_date(order_date, "%d-%m-%Y");
SELECT firstname, Total_sales, order_date FROM
(SELECT firstname, Total_sales, order_date , dense_rank() over (order by Total_sales desc) as Rnk FROM 
(SELECT firstname, max(order_quantity * Order_cost) as Total_sales, order_date
FROM Customers c,Orders o
WHERE c.id = cust_id and order_date >="2021-02-01" and order_date <="2021-03-01"
Group by cust_id) as tt) as tt1
WHERE Rnk=1 ;


SELECT firstname, max(order_quantity * Order_cost) as Total_sales, order_date
FROM Customers c,Orders o
WHERE c.id = cust_id and order_date >="2021-02-01" and order_date <="2021-03-01"
Group by cust_id;

/*Use the given data to find the total number of downloads for paying and non-paying users by date. 

Include only records where non-paying customers have more downloads than 
paying customers. The output should be sorted by earliest date first and contain 3 columns 
date, non-paying downloads, paying downloads.*/
SELECT * FROM account_details;
SELECT * FROM download_details;
SELECT * FROM user_details;
SELECT date, non_paying_downloads,paying_downloads FROM
(SELECT date, sum(case when Paying_customers="No" then downloads end) as non_paying_downloads, sum(case when Paying_customers="Yes" then downloads end) 
as paying_downloads
FROM user_details u, download_details d, account_details a
WHERE u.user_id = d.user_id and u.acc_id = a.acc_id 
Group by date) as tt
Having non_paying_downloads > paying_downloads
order by date;

/*Use the given data to find the popularity percentage for each user on Facebook. 
The popularity percentage is defined as the total number of friends 
the user has divided by the total number of users on the platform,
 then converted into a percentage by multiplying by 100. 
 Output each user along with their popularity percentage. 
 Order records in ascending order by user id. 
 The 'user1' and 'user2' column are pairs of friends.*/
 SELECT * FROM fb_friends;
 /*Deleting repetitive pairs*/
Create view delfb as (
SELECT DISTINCT user1,user2
FROM fb_friends f1
WHERE f1.user1 > f1.user2
    OR NOT EXISTS (
        SELECT * FROM fb_friends f2
            WHERE f2.user1 = f1.user2 AND f2.user2 = f1.user1
    )
    );
    SELECT  * FROM delfb;
    SET @Totalusers := (Select distinct Count(*) from (SELECT user1 from delfb
 UNION 
 SELECT user2 from delfb) as tt);
     
     SELECT user1 as user, count(*) as friends,round((count(*)/@Totalusers)*100,2) as pop_percent FROM (
     SELECT user1 from delfb
 UNION ALL
 SELECT user2 from delfb) as tt
 Group by user1
 Order by user1;

 /*Marketing_Campaign data
You have a table of in-app purchases by user. Users that make their
 first in-app purchase are placed in a marketing campaign where they see 
 call-to-actions for more in-app purchases of products . Find the number of users that
 made additional in-app purchases due to  the marketing campaign.


The marketing campaign doesn't start until one day after the initial
 in-app purchase so users that make multiple purchases on
 the same day do not count, nor do we count users that
 make only the same purchases over time.*/
 SELECT * FROM Marketing_campaign;
 SELECT count(DISTINCT user_id) as No_of_users
FROM marketing_campaign
WHERE user_id in
    (SELECT user_id
     FROM marketing_campaign
     GROUP BY user_id
     HAVING count(DISTINCT created_at) >1
     AND count(DISTINCT product_id) >1)
  AND concat((user_id),'_', (product_id)) not in
    (SELECT user_prod
     FROM
       (SELECT *,
               rank() over(PARTITION BY user_id
                           ORDER BY created_at) AS rnk,
               concat((user_id),'_', (product_id)) AS user_prod
        FROM marketing_campaign) as tt
     WHERE rnk = 1 );
     
     
     
     
     select 
count(distinct user_id)
-- *
from
(select 
dense_rank() over (partition by user_id order by created_at) as rnk1, 
dense_rank() over (partition by user_id , product_id order by created_at) as rnk2, 
dense_rank() over (partition by user_id , created_at order by product_id) as rnk3,
 user_id, 
 product_id, 
 created_at
from marketing_campaign) foo
where rnk1>1 and rnk2=1 and rnk3=1;


select count(distinct user_id) from (SELECT user_id,min(created_at) over(partition by user_id ) as m1,
 min(created_at) over(partition by user_id,product_id ) as m2 FROM marketing_campaign)c where m1<>m2;
