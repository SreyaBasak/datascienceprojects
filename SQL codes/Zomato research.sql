CREATE DATABASE Zomato;
Use Zomato;
CREATE TABLE Zomato_rest(
name varchar(500),	
online_order varchar(10),	
book_table varchar(10),	
rate FLOAT,	
votes INT,
rest_type varchar(100),
dish_liked varchar(500),
cuisines varchar(500),
avg_cost INT,
meal_type varchar(50),	
city varchar(100)
);
SELECT * FROM Zomato_rest;

/*No. of restaurants having the option for online ordering*/ 
SELECT count(online_order) as Total, sum(online_order = 'Yes') as YES, (sum(online_order = 'Yes'))/(count(online_order))*100 as `Yes %`,
sum(online_order = 'No') as NO, (sum(online_order = 'No'))/(count(online_order))*100 as `No %`
FROM Zomato_rest;

/*No. of restaurants having the option to book a table*/
SELECT count(book_table) as Total, sum(book_table = 'Yes') as YES, (sum(book_table = 'Yes'))/(count(book_table))*100 as `Yes %`,
sum(book_table = 'No') as NO, (sum(book_table = 'No'))/(count(book_table))*100 as `No %`
FROM Zomato_rest;

/*No. of restaurants with meal types*/
SELECT Meal_Type, count(*) as COUNT
FROM Zomato_rest
Group by meal_type;

/*No. of restaurants in each city*/
SELECT  City, count(*) as Count, dense_rank() over (Order By count(*) DESC) as DRank
FROM Zomato_rest
Group by city;

/*City with the highest no. of restaurants*/
SELECT City, Count 
FROM
(SELECT City, count(*) as Count, dense_rank() over (Order By count(*) DESC) as DRank
FROM Zomato_rest
Group by city) as tt
WHERE DRank = 1;

/*City with the lowest no. of restaurants*/
SELECT City, Count 
FROM
(SELECT City, count(*) as Count, dense_rank() over (Order By count(*) ASC) as DRank
FROM Zomato_rest
Group by city) as tt
WHERE DRank = 1;

/*No. of restaurants for each rating*/
SELECT rate, count(*) as Count
FROM Zomato_rest
Group by rate
Order by count(*) DESC;

/*No. of restaurant for each type*/
SELECT rest_type, count(*) as COUNT
FROM Zomato_rest
Group by rest_type;

/*Top 10 types of restaurant which have the highest no. of restaurant*/
SELECT rest_type, count
FROM
(SELECT rest_type, count(*) as COUNT, dense_rank() over (Order By count(*) DESC) as DRank
FROM Zomato_rest
Group by rest_type) as tt
WHERE DRank <=10;

/*10 types which have the least number of restaurants*/
SELECT rest_type, count
FROM
(SELECT rest_type, count(*) as COUNT, rank() over (Order By count(*) ASC) as Rnk
FROM Zomato_rest
Group by rest_type) as tt
WHERE DRank <=10;

/*Number of restaurants for different average costs*/
SELECT Avg_cost, count(*) as COUNT
FROM Zomato_rest
Group by Avg_cost
Order by count(*) DESC,Avg_cost;

/*Top 10 cuisines that are offered by the maximum number of restaurants.*/
SELECT cuisines, COUNT 
FROM
(SELECT cuisines, count(*) as COUNT, dense_rank() over (Order By count(*) DESC) as Rnk
FROM Zomato_rest
Group by cuisines) as tt
WHERE Rnk <=10;

/* 10 least served cuisines in Bangalore  need to be checked*/
SELECT cuisines, COUNT 
FROM
(SELECT cuisines, count(*) as COUNT, rank() over (Order By count(*) ASC) as Rnk
FROM Zomato_rest
Group by cuisines) as tt
WHERE Rnk <=10 and cuisines Not Like "%North Indian%" and cuisines Not Like "%South Indian%" and cuisines Not Like "%Chinese%" and cuisines Not Like "%ice Cream%" and cuisines Not Like "%Biryani%";

SELECT cuisines, COUNT 
FROM
(SELECT cuisines, count(*) as COUNT, rank() over (Order By count(*) ASC) as Rnk
FROM Zomato_rest
Group by cuisines) as tt
WHERE Rnk <=10 and cuisines RegExp "[^North Indian,South Indian,Chinese,ice Cream]" ;

/* Top 50 Restaurants that are highly voted*/
SELECT name, votes
FROM
(SELECT name, votes, dense_rank() over (Order By votes DESC) as Rnk
FROM Zomato_rest
Group by votes) as tt
WHERE Rnk <=50;

/*Top 50 rated restaurants.*/
SELECT distinct name, rate
FROM Zomato_rest
order by rate desc
LIMIT 50;

/*50 least rated restaurants.*/
SELECT Distinct name, rate
FROM Zomato_rest
order by rate
LIMIT 50;

/* First 50 restaurants ordered by avg_cost feature */
SELECT distinct name, avg_cost
FROM Zomato_rest
Order by Avg_cost desc
LIMIT 50;

/*Number of restaurants for each rating where you can or can not book order online*/
SELECT rate, sum(online_order = 'Yes') as Online_order_Yes, sum(online_order = 'No') as Online_order_No
FROM Zomato_rest
Group by rate
Order by rate DESC;

/* number of restaurants for each rating as per the values(yes or no) for book_table feature*/
SELECT rate, sum(book_table = 'Yes') as book_table_Yes, sum(book_table = 'No') as book_table_No
FROM Zomato_rest
Group by rate
Order by rate DESC;

/*number of restaurants for each city which do or do not have the facility to book order online*/
SELECT city, sum(online_order = 'Yes') as Online_order_Yes, sum(online_order = 'No') as Online_order_No
FROM Zomato_rest
Group by city;

/*which city is costlier in terms of food, and which are not with respect to average cost for each city*/
SELECT city, Round(avg(avg_cost),0) as Average_Cost
FROM Zomato_rest
Group by city
Order by avg(avg_cost) desc;

/*which city has the highest number of restaurants with high ratings.*/
SELECT city, Round(avg(rate),2) as Ratings
FROM Zomato_rest
Group by city
Order by avg(rate) desc;

/*Top 10 dishes liked by the citizens of Bangalore.*/
SELECT dish_liked, count(*) as COUNT
FROM Zomato_rest
WHERE dish_liked not like "Friendly staff" and dish_liked not like "Rooftop Ambience"
Group by dish_liked
order by count(*) desc
LIMIT 10;


/*Top restaurants serving Most liked dish 'Biryani' , along with their ratings and avg_cost*/
SELECT distinct name, rate, avg_cost
FROM Zomato_rest
WHERE dish_liked = "Biryani"
order by rate desc;

SELECT count(*) FROM Zomato_rest;
