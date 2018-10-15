#---------------------------------------------
#Task 1: Understanding the data in hand
#---------------------------------------------


#A. Describe the data in hand in your own words. (Word Limit is 500)

# market_fact is a fact table, and all other tables are dimensional tables. We can do different kind of analysis using Dimensional tables.
# The design of this schema is similar to Star Schema Data warehouse design.
# 1. market_fact => it contains all the transactional information. In an Ecommerce website a Customer can place multiple order and each 
# order can contain multiple products. Each order can contain different number of quantity of each product. if one order contains multiple products
# then it can be shipped in different shipments.
# 2. cust_dimen => what customer_segment/province/region  should be focused, based on the transaction done by them.
# 3. Orders_dimen => contains the priority based info for the order placed.
# 4. Shipping_dimen => delivery mode of a product based on the priority of an order.
# 5. Prod_dimen => different categories and sub-category of a product


#B. Identify and list the Primary Keys and Foreign Keys for this dataset 
#(Hint: If a table don’t have Primary Key or Foreign Key, then specifically mention it in your answer.)

#1. cust_dimen => Primary Key -> Cust_id,
	#Foreign Key => No Foreign Key
#2. market_fact => Primary Key -> combination of(Ord_id,Prod_id,Ship_id,Cust_id) , 
# 	Foreign Key -> Ord_id references to Orders_dimen.Ord_id, Prod_id references to Prod_dimen.Prod_id , Ship_id references to Shipping_dimen.ship_id, Cust_id references to cust_dimen.Cust_id
#3. Orders_dimen => Primary Key -> Ord_id,
#	Foreign key -> No Foreign key
#4. Prod_dimen => Primary Key -> Prod_id,
#	Foreign Key -> No Foreign key
#5. Shipping_dimen => Primary key -> Ship_id,
#	Foreign key ->  No Foreign key

#---------------------------------------------
#Task 2: Basic Analysis
#---------------------------------------------

#A. Find the total and the average sales (display total_sales and avg_sales) 

 select sum(sales) as "sum",avg(sales) as "average" from superstoresDB.market_fact;

#B. Display the number of customers in each region in decreasing order of
#no_of_customers. The result should contain columns Region, no_of_customers

select Region,count(distinct cust_id) as no_of_customers from superstoresDB.cust_dimen group by region order by no_of_customers desc;

#C. Find the region having maximum customers (display the region name and max(no_of_customers)

select Region, count(distinct cust_id) as no_of_customers from superstoresDB.cust_dimen group by region order by no_of_customers desc limit 1;

#D. Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold)

select prod_id as "productId", sum(order_quantity) as no_of_products_sold from market_fact group by prod_id order by no_of_products_sold desc;

#E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and
#the number of tables purchased (display the customer name, no_of_tables purchased) 

select customer.customer_name as customerName, sum(market.order_quantity) as num_of_table_purchased from
(select customer_name,cust_id,region from cust_dimen where region = "Atlantic") customer,
(select prod_id,cust_id,order_quantity from market_fact  ) market,
(select product_sub_category,prod_id from prod_dimen where product_sub_category="TABLES") product
where customer.cust_id=market.cust_id
and product.prod_id = market.prod_id
group by customer.customer_name;


#---------------------------------------------
#Task 3: Advanced Analysis
#---------------------------------------------

#A. Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)

select product.product_category,sum(market.profit) as "profits" from
(select profit,prod_id from market_fact) market,
(select product_category,prod_id from prod_dimen) product
where market.prod_id=product.prod_id
group by product.product_category order by profits desc;

#B. Display the product category, product sub-category and the profit within each subcategory in three columns.

select product.product_category,product.product_sub_category,sum(market.profit) as "profit" from
(select profit,prod_id from market_fact) market,
prod_dimen product
where market.prod_id=product.prod_id
group by product.product_category,product.product_sub_category ;


#C. Where is the least profitable product subcategory shipped the most. For the least profitable product sub-category, display the region-wise no_of_shipments and the
#profit made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region)
#Note: You can hardcode the name of the least profitable product subcategory



#C.1 Where is the least profitable product subcategory shipped the most


select customer.Region,count(market.ship_id) as shipcount
from cust_dimen customer,market_fact market     
where customer.cust_id = market.cust_id
and market.prod_id= 
(select prod_id from prod_dimen where product_sub_category = (select product_sub_category from (select product.product_category,product.product_sub_category as product_sub_category,sum(market.profit) as "profit" from
(select profit,prod_id from market_fact) market,
prod_dimen product
where market.prod_id=product.prod_id
group by product.product_category,product.product_sub_category order by profit)tbl order by profit limit 1))
 
group by customer.region order by shipcount desc limit 1;



#C.2 For the least profitable product sub-category, display the region-wise no_of_shipments and the
#profit made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region)


select customer.Region,count(market.ship_id) as shipcount,sum(market.profit) as profit
from cust_dimen customer,market_fact market     
where customer.cust_id = market.cust_id
and market.prod_id= 

(select prod_id from prod_dimen where product_sub_category = (select product_sub_category from (select product.product_category,product.product_sub_category as product_sub_category,sum(market.profit) as "profit" from
(select profit,prod_id from market_fact) market,
prod_dimen product
where market.prod_id=product.prod_id
group by product.product_category,product.product_sub_category order by profit)tbl order by profit limit 1))
 
group by customer.region order by profit ;
#---------------------------------------------
#Task 2: Basic Analysis
#---------------------------------------------

#A. Find the total and the average sales (display total_sales and avg_sales) 

 select sum(sales) as "sum",avg(sales) as "average" from superstoresDB.market_fact;

#B. Display the number of customers in each region in decreasing order of
#no_of_customers. The result should contain columns Region, no_of_customers

select Region,count(distinct cust_id) as no_of_customers from superstoresDB.cust_dimen group by region order by no_of_customers desc;

#C. Find the region having maximum customers (display the region name and max(no_of_customers)

select Region, count(distinct cust_id) as no_of_customers from superstoresDB.cust_dimen group by region order by no_of_customers desc limit 1;

#D. Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold)

select prod_id as "productId", sum(order_quantity) as no_of_products_sold from market_fact group by prod_id order by no_of_products_sold desc;

#E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and
#the number of tables purchased (display the customer name, no_of_tables purchased) 

select customer.customer_name as customerName, sum(market.order_quantity) as num_of_table_purchased from
(select customer_name,cust_id,region from cust_dimen where region = "Atlantic") customer,
(select prod_id,cust_id,order_quantity from market_fact  ) market,
(select product_sub_category,prod_id from prod_dimen where product_sub_category="TABLES") product
where customer.cust_id=market.cust_id
and product.prod_id = market.prod_id
group by customer.customer_name;


#---------------------------------------------
#Task 3: Advanced Analysis
#---------------------------------------------

#A. Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)

select product.product_category,sum(market.profit) as "profits" from
(select profit,prod_id from market_fact) market,
(select product_category,prod_id from prod_dimen) product
where market.prod_id=product.prod_id
group by product.product_category order by profits desc;

#B. Display the product category, product sub-category and the profit within each subcategory in three columns.

select product.product_category,product.product_sub_category,sum(market.profit) as "profit" from
(select profit,prod_id from market_fact) market,
prod_dimen product
where market.prod_id=product.prod_id
group by product.product_category,product.product_sub_category ;


#C. Where is the least profitable product subcategory shipped the most. For the least profitable product sub-category, display the region-wise no_of_shipments and the
#profit made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region)
#Note: You can hardcode the name of the least profitable product subcategory



#C.1 Where is the least profitable product subcategory shipped the most


select customer.Region,count(market.ship_id) as shipcount
from cust_dimen customer,market_fact market     
where customer.cust_id = market.cust_id
and market.prod_id= 
(select prod_id from prod_dimen where product_sub_category = (select product_sub_category from (select product.product_category,product.product_sub_category as product_sub_category,sum(market.profit) as "profit" from
(select profit,prod_id from market_fact) market,
prod_dimen product
where market.prod_id=product.prod_id
group by product.product_category,product.product_sub_category order by profit)tbl order by profit limit 1))
 
group by customer.region order by shipcount desc limit 1;



#C.2 For the least profitable product sub-category, display the region-wise no_of_shipments and the
#profit made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region)


select customer.Region,count(market.ship_id) as no_of_shipments,sum(market.profit) as  profit_in_each_region
from cust_dimen customer,market_fact market     
where customer.cust_id = market.cust_id
and market.prod_id= 

(select prod_id from prod_dimen where product_sub_category = (select product_sub_category 
from (select product.product_sub_category as product_sub_category,sum(market.profit) as "profit" from
(select profit,prod_id from market_fact) market,
prod_dimen product
where market.prod_id=product.prod_id
group by product.product_sub_category order by profit)tbl order by profit limit 1))
 
group by customer.region order by  profit_in_each_region ;

