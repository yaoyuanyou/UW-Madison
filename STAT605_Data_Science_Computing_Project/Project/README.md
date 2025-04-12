# NYC Parking Violation Analysis

This is my course project in statistics program in UW-Madison. Thanks to my teammates [Hao Tong](https://github.com/htong25), [Junxia Zhao](https://github.com/jzhao347) and Jiayi Shen.

The data can be found on [NYC Department of Finance, NYC Open Data](https://data.cityofnewyork.us/City-Government/Parking-Violations-Issued-Fiscal-Year-2021/pvqr-7yc4).

## Introduction

Parking is pretty much a way of life in NYC. Meanwhile, parking violations have always been a great cause for traﬃc jam nowadays. Can we find some useful infomation to mitigate and help NYC government to make data-driven decisions? 
Below are some points that we are curious about:
- Is there a diﬀerence in violation amounts between weekdays and weekends? Any seasonality?
- Does plate registration state or vehicle body type have a relationship with parking violations?

And our workflow is:
- We program in **R** to conduct the student’s **t-test** to compare violation amounts between weekdays and weekends across all year.
- We apply **serial correlation** to analyze seasonal patterns.
- We utilize **data visualization** to show which registration state’s car and county cause more violations and defined the relationship between body type and violation code.

## Data Description

The dataset contains **45 Million** parking tickets data issued from July 2014 to June 2018. There are tens of columns in each of the 4 huge files.

The total data is about 10.27 GB. In order to compute in a simple way and save time for a huge dataset, we decide to use **CHTC** to conduct parallel computation for data processing.

## Feature Selection & Engineering

We successfully reduce the dimensions and extract 5 useful and shared variables that are related to our scope. They are `Registration.State`, `Issue.Date`, `Violation.Code`, `Vehicle.Body.Type`, and `Violation.County`.

## Statistical Finding

1.Daily Effect

After performing **Welch two sample t-test**, there is sufficient evidence violations are statistically different between weekdays and weekends with the `p-value` $< 2 * 10^{-16}$. The **EDA** looks like this:

2.Seasonality

We conbine 4 files into continuous, sequential data from July 2014 to June 2018.
![Violation Data From July 2014 to June 2018](https://github.com/yaoyuanyou/UW-Madison/blob/92c0da8cf6571172043461c06d3ec64e3af3b119/STAT605_Data_Science_Computing_Project/Project/img/seasonality.png)

It is worth mentioning that a snowstorm attacked NYC in January 2015, leading a high violation rate.

An **ARIMA** model is applied for this timeseries data and is check using **ACF**. After carefully parameters selection, a prediction is made and shown below:

![ARIMA Model Evaluation](https://github.com/yaoyuanyou/UW-Madison/blob/92c0da8cf6571172043461c06d3ec64e3af3b119/STAT605_Data_Science_Computing_Project/Project/img/ARIMA-eval.png)

![ARIMA Model Prediction](https://github.com/yaoyuanyou/UW-Madison/blob/92c0da8cf6571172043461c06d3ec64e3af3b119/STAT605_Data_Science_Computing_Project/Project/img/ARIMA-pred.png)

3. Geography
4. Vehicle Types & Violation Codes

We confirm vehicle types and violation types are related. By conducting **Chi-squared test**. We also examine the top 5 vehicle body types with the most violations.

![Top Violation According to Body Type](https://github.com/yaoyuanyou/UW-Madison/blob/92c0da8cf6571172043461c06d3ec64e3af3b119/STAT605_Data_Science_Computing_Project/Project/img/vehicle-type.png)

Given different body types, we check the violation types and visualize as below:
![Violation Distribution According to Body Type](https://github.com/yaoyuanyou/UW-Madison/blob/92c0da8cf6571172043461c06d3ec64e3af3b119/STAT605_Data_Science_Computing_Project/Project/img/violation-code.png)

## Conclusion

From the analysis, we suggest that police duty schedule can be optimized based on weekly, seasonality and geographic patterns. Besides, we suggest NYC can do better to ensure drivers be aware of No Parking Zone. Furthermore, our result is meaningful for insurance companies since several factors aﬀect the vehicle insurance premium.
