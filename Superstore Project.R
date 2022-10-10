# SUPER STORE ANALYSIS PROJECT

#Loading Libraries 
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(readxl)
library(treemapify)


#Importing and reading the data frame

superstore <- read_excel("Superstore.xls", 
                         col_types = c("numeric", "text", "date", 
                                       "date", "text", "text", "text", "text", 
                                       "text", "text", "text", "numeric", 
                                       "text", "text", "text", "text", "text", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric"))


##EXPLORATORY DATA ANALYSIS



#Getting to know my data looking at the first few rows
head(superstore)

# the structure of the data
glimpse(superstore)




#Cleaning up the variable names
superstore <- clean_names(superstore)

# Checking my variables or column name of the datframe
colnames(superstore)


# Checking out the dimension of the dataframe
dim(superstore)

## there are 9994 rows or observation and 21 columns or variables,in the dataframe.



#Checking for missing value in the dataframe
sum(is.na(superstore))


##There are zero missing value in the dataframe


#checking out the datatype of the variables
sapply(superstore, class)

# Getting the summary of the numeric variables
summary(superstore)

## we would not need the row_id and the country variable in the
## analysis, since the data is for only united state

#Dropping row_id and country
superstore <- within(superstore, rm(row_id, country))

#confirming the removal of rowid and country variable
superstore %>% 
  head(10)



##ANALYSIS

## Analysis of the superstore will be carried out based on;

# a. Product level analysis
# b. Customer level Analysis
# c. Region and Time series Analysis

#------------------------------------------------------------------------
# A. PRODUCT LEVEL ANALYSIS

# Let's look at the product category available
unique(superstore$category)

# Number of products in each category
superstore %>% 
  group_by(category) %>% 
  summarise(total=n()) %>% 
  arrange(desc(total))

# sub_category products are divided into;
n_distinct(superstore$sub_category)


#number of products in each sub-category
superstore %>% 
  group_by(sub_category) %>% 
  summarise(total=n()) %>% 
  arrange(desc(total))



## The distribution of subcategory
#Category by Sub_category grouping
ggplot(superstore, aes(x= sub_category, y = category, fill=category)) + 
  geom_col(position = position_dodge(),show.legend = FALSE)+ coord_flip()+ 
  theme_classic() + labs(title = "Category by Sub_Category",
                         subtitle ="Distribution of Subcategory")+
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))


## From this graph, one can easily decide which Category 
## & Sub-Category to lookout for when purchasing a product
## from the store

#----------------------------------------------------------- 
#Count by Category and Subcategory

ggplot(superstore, aes(x= sub_category, fill = category)) + 
geom_bar() + theme_bw() + coord_flip() + 
labs(title = "Count by Category and Sub_Category",
       x = "Sub_Category", y = "Frequency")+
theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))

##The store has wide variety of Office Supplies 
##especially in Binders and Paper

#total profit and sale by sub_category
superstore %>% 
  group_by(sub_category) %>% 
  summarise( total_sales = sum(sales),total_profit =sum(profit)) %>% 
  pivot_longer(c("total_sales", "total_profit")) %>% 
  ggplot(aes(x=sub_category, y = value, fill = name)) + 
  geom_col(position = position_dodge()) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Total Profit and Sales by Sub_Category") +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))
  


## Highest profit is earned in Copiers while the sales of 
##Chairs and Phones is high when compared to other products.
##However, the profit on Tables and Bookcases is not significant 
##Hence, these departments are in loss.




# Total number of products available in the store is;
n_distinct(superstore$product_name)


#Distribution of top 10 product
superstore %>% 
  group_by(product_name) %>% 
  summarize(count_of_products=n()) %>% 
  arrange(desc(count_of_products)) %>% 
  slice_head(n=10) %>% 
  ggplot(superstore, 
         mapping = aes(x= product_name,
                       y= count_of_products))+
   geom_segment(aes(x = reorder(product_name, count_of_products), xend = product_name,
                   y = 0, yend = count_of_products), size =1,)+
  geom_point(size=10, color= "orange",
             shape = 20, stroke = 2) + coord_flip() + theme_bw() +
  labs(title = "Distribution of Top 10 Product",
       x = NULL) +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5)) +
  geom_text(aes(label= count_of_products), 
            color="black", size=4, vjust=0.5, fontface='bold')
  


#Count of Sub-Category by region
superstore %>% 
  ggplot(aes(x=sub_category, fill= region)) + 
  geom_bar(position = position_dodge(width = 0.7))+
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Count of Sub_category Product Sales by Region")+
  theme(plot.title = element_text(size=20, face="bold",hjust = 0.5))
  
##People residing in Western part of US 
##tend to order more from superstore.



##To analyze further, we are going to calculate the cost and profit percent variables


# Calculating Cost
superstore <- superstore %>% 
  mutate(cost = sales - profit)
  

##showing the first ten row of the new dataframe
superstore %>% 
  head(10)


## Top 10 product by Cost
superstore %>% 
  select(product_name, cost) %>% 
  slice(1:10) %>% 
  arrange(desc(cost))
#---------------------------------------------------------------------

#Also, calculating percentage profit
superstore <- superstore %>% 
  mutate(profit_percent = (profit/cost *100))

#Category and Products with 100% profits
superstore %>% 
  select(category, product_name, profit_percent) %>% 
  filter(profit_percent==100) %>% 
  arrange(category)

##140 products got 100% profits from the three category,
##However, office supplies have more product with 100% profit
##as further shown by the bar chart below

superstore %>% 
  select(category, product_name, profit_percent) %>% 
  filter(profit_percent==100) %>% 
  arrange(category) %>% 
  ggplot(aes(x= category, fill = "Red")) + 
  geom_bar(show.legend = FALSE)+ theme_bw() +
  labs(title = "Count of Category with 100% profit")+
  theme(plot.title = element_text(size=20, face="bold",hjust = 0.5))


#-----------------------------------------------------------------------------
## CUSTOMER LEVEL ANALYSIS

# Total Customer count
n_distinct(superstore$customer_name)

#Top Ten Customer who ordered frequently from the store
superstore %>% 
  group_by(customer_name) %>% 
  summarise(top_10_customer=n()) %>% 
  arrange(desc(top_10_customer)) %>% 
  slice_head(n=10)

## The most Common ship_mode and segment used by customers
superstore %>% 
  group_by(ship_mode, segment) %>% 
  summarise(total=n()) %>% 
  ggplot(aes(x=ship_mode, y = total, fill= segment)) + 
  geom_col(position=position_dodge()) + facet_wrap(~segment)+ theme_bw()+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = "Distribution by Segment and Ship Mode") +
    theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))+
  geom_text(aes(ship_mode, label = total),vjust = -0.4)
  
#The distribution is highest in Consumer Segment and Ship_mode for 
#standard class were highest for all segment


#Top 20 Customers by profit and by State

superstore %>% 
  mutate(summarise(superstore,
                   profit_2 = round(profit, digits = 0))) %>% 
  select(customer_name, state, profit_2) %>% 
  arrange(desc(profit_2)) %>% 
  slice(1:20) %>% 
  ggplot(aes(x= reorder(customer_name, profit_2), 
             y= profit_2, fill = reorder(state, -profit_2))) + 
  geom_col() + coord_flip() + theme_classic() +
  labs(title = "Top 20 Customers by Profit and State",
       x= "Customer Name", y = "Profit (Dollars)") +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5)) +
  geom_text(aes(customer_name, label=profit_2),
            size = 2.9,hjust = -0.01, fontface= "bold")

#The most profitable customer is from Indiana followed 
#by Washington. However, we could see that majority of Profitable 
#Customers are from New York and Michigan State.
#-------------------------------------------------------------------



#TOP 10 CUSTOMER by Sale and by state
superstore %>% 
  mutate(summarise(superstore, 
                   sales_2 = round(sales, digits = 0))) %>% 
  select(customer_name, state, sales_2) %>% 
  arrange(desc(sales_2)) %>% 
  slice(1:10) %>% 
  ggplot(aes(x=reorder(customer_name, -sales_2), 
             y= sales_2, fill= reorder(state, -sales_2)))+
  geom_col() + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
labs(title = "Top 10 Customers by Sales and State",
     x= "Customer Name", y = "Total Sales") +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))+
  theme(legend.title = element_blank()) +
  geom_text(aes(label = sales_2),
             vjust=0.1, alpha=1, size = 4, fontface= "bold")
 


#Top Ten Customers by Sales ---- choose one by state or by sales only.
superstore %>% 
  mutate(summarise(superstore, 
                   sales_2 = round(sales, digits = 0))) %>% 
  select(customer_name, sales_2) %>% 
  arrange(desc(sales_2)) %>% 
  slice(1:10) %>% 
  ggplot(aes(x=reorder(customer_name, -sales_2), 
             y= sales_2, fill= customer_name))+
  geom_col(show.legend = FALSE) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title = "Top 10 Customers by Sales ",
       x= "Customer Name", y = " Sales (Dollars)") +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5)) +
  geom_text(aes(label = sales_2),
  
            
                       vjust= 0.01, size = 4, fontface= "bold")
#------------------------------------------------------------------------
# REGION & TIME SERIES ANALYSIS


## TOTAL SALES AND PROFIT BY REGION


# Sales by Region
superstore %>% 
  group_by(region) %>% 
  summarize(total_sales = round(sum(sales))) %>% 
  ggplot(aes(area = total_sales, fill = region, 
             label= paste0(region, "\n",prettyNum(total_sales, ",")))) +
  geom_treemap() + geom_treemap_text(color= "white", 
                    place="centre", fontface = "bold", size = 25) +
  theme(legend.position = "none")+ labs(title = "Sales by Region") + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))
## The west generated the highest 
##Sales by Region with the total sales of $725,458


# Profit by Region 
superstore %>% 
  group_by(region)%>%
  summarize(total_profit = round(sum(profit))) %>% 
  ggplot(aes(area = total_profit  , fill = region, 
             label= paste0(region, "\n",prettyNum(total_profit, ",")))) +
  geom_treemap() + 
  geom_treemap_text(color= "white", 
                    place="centre", fontface = "bold", size = 25) +
  theme(legend.position = "none") + labs(title = "Profit by Region") + 
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))

## The west generated the highest 
##profit by region with the total profit of $108,418  

#------------------------------------------------------------------------------
## We shall be calculating shipment duration as well
## extracting order year from the data frame.


#Calculating shipment duration

superstore <- superstore %>% 
  mutate(duration_to_ship = 
           as.Date(superstore$ship_date) - as.Date(superstore$order_date)) %>% 
  arrange(desc(duration_to_ship))


#Highest shipment duration and ship mode by top 20 product
superstore %>% 
  select(product_id, ship_mode,duration_to_ship) %>% 
  arrange(desc(duration_to_ship)) %>% 
  slice(1:20)

## From the above table, it takes maximum 7 days or one week using the 
##second class ship_mode to ship a product out from the store.
  

##Average shipping duration by ship mode

superstore %>% 
  group_by(ship_mode) %>% 
  summarise(avg_duration = round(mean(duration_to_ship))) %>%
  ggplot(aes(x= ship_mode,
                       y= avg_duration, color = ship_mode ))+
  geom_segment(aes(x = reorder(ship_mode, avg_duration), 
                   xend = ship_mode, y = 0, yend = avg_duration), size =2,
               show.legend = FALSE) +
  geom_point( size=10, shape = 20, stroke = 2, show.legend = FALSE)+ 
  coord_flip() + theme_bw()+
  labs(title = "Average shipping duration by ship mode",
       x = "Ship Mode", y= "Average Duration (days)") +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5)) +
  geom_text(aes(label= avg_duration), 
            color="black", size=4, vjust=0.5, fontface='bold')

##From the above, same day shipping mode, ship the product on the day
##it was ordered for, However, it will take an average of 5 days
##by the standard class ship mode 
  
#-----------------------------------------------------------------------------
# Extracting year from the order date data frame

superstore <- superstore %>% 
  mutate(order_year = year(order_date))

##Profit and sales by year
superstore %>% 
  group_by(order_year) %>% 
  summarize(total_sales = round(sum(sales)), 
            total_profit= round(sum(profit))) %>% 
  pivot_longer(c("total_sales", "total_profit")) %>% 
  ggplot(aes(x= order_year, y= value, fill = name))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels=scales::comma) + theme_bw()+
geom_text(aes(order_year, label = value), vjust =0.001, 
           size = 3, fontface= "bold",
          position = position_dodge(width = 1))+
labs(title = "Profit and Sale by Year",
     x= "Year", y = "Value (dollars)") +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))+
  theme(legend.title = element_blank())
## The above shows that sales and profit
## increased for each year resulting in high
## Profit margin in year 2017



##profit and sales by year for each region
superstore %>% 
  group_by(order_year,region) %>% 
  summarize(Total_Sales = round(sum(sales)), 
            Total_Profit= round(sum(profit))) %>% 
  pivot_longer(c("Total_Sales", "Total_Profit")) %>%
  ggplot(aes(x= order_year, y= value, fill = name))+
  geom_col(position = position_dodge(width = 1)) + facet_wrap(~region)+
  theme_bw()+
  geom_text(aes(order_year, label = value), vjust =0.001, 
            size = 2.5, fontface= "bold",
            position = position_dodge(width = 1))+
  labs(title = "Sales and Profit by Year for each Region",
     x= "Year", y = "Value (dollars)")+
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))+
  theme(legend.title = element_blank())
## The above shows that the west Region have the highest total
## profit and sales across the year from 2014 to 2017. However,
## in the year 2015, the east has more profit and sales more than 
## any other region including the west.


# Sales by Category for each year
superstore %>% 
  group_by(category, order_year) %>% 
  summarize(total_sales=sum(sales)) %>% 
  ggplot(aes(x=category, y= total_sales, fill = category)) +
  geom_col() + facet_grid (~order_year) + theme_bw()+
  scale_y_continuous(labels=scales::comma) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Sales by Category for each Year",
       x= NULL, y = "Sales (dollars)")+
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))
##From the above, Technology products have the highest sales in
##year 2014, 2016 and 2017. However, more of furniture products were sold
## in the year 2015.


# PROFIT TREND BY CATEGORY FOR EACH YEAR
superstore %>% 
  group_by(category, order_year) %>% 
  summarize(total_profit=sum(profit)) %>% 
  ggplot(aes(x= order_year, y=total_profit, color = category)) + 
  geom_point(size = 5, alpha=0.5) + geom_line(size=1) +
  theme_bw() +
  labs(title = "Profit Trend by Year for each Category",
       x= "Year", y = "Profit (dollars)")+
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5))
## There is consistence increase in profit in office supplies and
##technological products from Year 2014 to 2017. However, there is
## a huge drop in profit in year 2015 and 2017 from the sales of
## furniture products.


# Sales, Profit and Discount Relationship
superstore %>% 
ggplot(aes(x=sales, y=profit, color=discount))+
geom_point(size=3) + geom_rug() + theme_bw() +
  labs(title="Sales,Profit and Discount Relationship")+
  theme(plot.title=element_text(size=20, face="bold", hjust=0.5))
 ## The higher the sales, the higher the profit while
## the higher the discount, the lower the profit hence,
##Profit tends to negative.


#------------------- THE END ------------------------------#
