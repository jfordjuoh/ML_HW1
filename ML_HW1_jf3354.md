Machine Learning\_hw1\_jf3354
================
Judy Fordjuoh
January 21, 2022

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

## QUESTION 1

``` r
#Task: Construct a table providing summaries of the quantitative features of the dataset. Summaries should include the mean, median, minimum value, and maximum value. If you are unable to construct a formatted table within R, you can print raw output, but then comment the output to identify the answer that was requested.

bcdata = read_csv("bcdata_Assignment1.csv") %>%
    janitor::clean_names()
```

    ## Rows: 116 Columns: 10

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (10): Age, BMI, Glucose, Insulin, HOMA, Leptin, Adiponectin, Resistin, M...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
summary(bcdata)
```

    ##       age            bmi           glucose          insulin      
    ##  Min.   :24.0   Min.   :18.37   Min.   : 60.00   Min.   : 2.432  
    ##  1st Qu.:45.0   1st Qu.:22.97   1st Qu.: 85.75   1st Qu.: 4.359  
    ##  Median :56.0   Median :27.66   Median : 92.00   Median : 5.925  
    ##  Mean   :57.3   Mean   :27.58   Mean   : 97.79   Mean   :10.012  
    ##  3rd Qu.:71.0   3rd Qu.:31.24   3rd Qu.:102.00   3rd Qu.:11.189  
    ##  Max.   :89.0   Max.   :38.58   Max.   :201.00   Max.   :58.460  
    ##       homa             leptin        adiponectin        resistin     
    ##  Min.   : 0.4674   Min.   : 4.311   Min.   : 1.656   Min.   : 3.210  
    ##  1st Qu.: 0.9180   1st Qu.:12.314   1st Qu.: 5.474   1st Qu.: 6.882  
    ##  Median : 1.3809   Median :20.271   Median : 8.353   Median :10.828  
    ##  Mean   : 2.6950   Mean   :26.615   Mean   :10.181   Mean   :14.726  
    ##  3rd Qu.: 2.8578   3rd Qu.:37.378   3rd Qu.:11.816   3rd Qu.:17.755  
    ##  Max.   :25.0503   Max.   :90.280   Max.   :38.040   Max.   :82.100  
    ##      mcp_1         classification 
    ##  Min.   :  45.84   Min.   :1.000  
    ##  1st Qu.: 269.98   1st Qu.:1.000  
    ##  Median : 471.32   Median :2.000  
    ##  Mean   : 534.65   Mean   :1.552  
    ##  3rd Qu.: 700.09   3rd Qu.:2.000  
    ##  Max.   :1698.44   Max.   :2.000

For the age category, the minimum age in the dataset is 24 years, the
median is 56 years, the average age (mean) is 57.3 years, and the
maximum age in this data set is 89 years. In the BMI category,the lowest
BMI in the dataset is 18.37 kg/m2, the median BMI is 27.66 kg/m2, the
average BMI is 27.58 kg/m2, and the highest BMI in this data set is
38.58 kg/m2. As for glucose, the lowest glucose level is 60.0 mg/dL, the
median is 92.00 mg/dL, the average glucose level is 97.79 mg/dL, and the
maximum glucose level in this dataset is 201.00 mg/dL. The lowest
insulin level is 2.432 μU/mL, the median is 5.925 μU/mL, the average
level is 10.012 μU/mL, and the maximum insulin level is 58.460 μU/mL.
The lowest result from the homeostatic model assessment was 0.4674, the
median was 1.3809, the mean was 2.6950, and the maxium result in this
dataset was 25.0503. The lowest level of leptin was 4.311 ng/mL, the
median was 20.271 ng/mL, the average level was 26.615 ng/mL, and the
maximum level of leptin in this dataset was 90.280 ng/mL. The lowest
level of adiponectin was 1.656 μg/mL, the median was 8.353 μg/mL, the
average level was 10.181 μg/mL, and the maximum level of adiponectin in
this dataset was 38.040 μg/mL. The lowest level of resistin was 3.210
ng/mL, the median was 10.828 ng/mL, the average level was 14.726 ng/mL,
and the maximum level of resistin in this dataset was 82.100 ng/mL. The
lowest chemokine level (MCP-1) was 45.84 pg/dL, the median was 471.32
pg/dL, the average level was 534.65 pg/dL, and the maximum chemokine
level in this dataset was 1698.44 pg/dL. Since the classification labels
is only two levels (1= controls and 2= breast cnacer patients), the
minimium = 1, the maximum =2, mean = 1.552, and the median = 2. Since
the classification labels is mainly used to group the patients, this
descriptive explaniation of this variable may not be as useful.

## QUESTION 2

``` r
#Task: Recode BMI into the WHO-defined categories below: Severely underweight - BMI less than 16.5kg/m^2, Underweight - BMI under 18.5 kg/m^2, Normal weight - BMI greater than or equal to 18.5 to 24.9 kg/m^2, Overweight – BMI greater than or equal to 25 to 29.9 kg/m^2, Obesity class I – BMI 30 to 34.9 kg/m^2, Obesity class II – BMI 35 to 39.9 kg/m^2, Obesity class III – BMI greater than or equal to 40 kg/m^2. 

#Note to self: there is no severely underweight in this data set
 
#Note to self: regrouping the BMI, referred to DS midterm

bcdata = bcdata %>%  
  mutate(bmi_category = cut(bmi, breaks = c(0, 16.5, 18.5, 24.9, 29.9, 34.9, 39.9, 50), labels = c("Severely Underweight","Under Weight", "Normal Weight","Overweight","Obesity Class  I","Obesity Class II", "Obesity Class III"))) %>%  
  select(bmi,bmi_category, everything()) 
```

After categorizing the individuals to the respective BMI categories, we
see that there is no study participants in the Severely Underweight and
the Obesity Class III BMI categories.

## QUESTION 3

``` r
#Task: Create a bar chart showing the proportion of breast cancer cases and controls within each BMI category

#Note to self: referred to DS midterm

dataclean = bcdata %>% 
  mutate(classification = as.character(classification)) %>%
  mutate(classification = recode(classification,
                       "1" = "Healthy Controls",
                       "2" = "Breast Cancer Patients")) %>% 
  group_by(bmi_category, classification) %>%
  summarize( 
    total = n()
    ) 
```

    ## `summarise()` has grouped output by 'bmi_category'. You can override using the `.groups` argument.

``` r
bcdata_graphs <- dataclean %>%
  ggplot(aes(x = classification, y = total, fill = classification)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  facet_grid(~bmi_category) +
  labs(
    title = "Proportion of Breast Cancer Cases and 
    Controls Within Each BMI Category",
    x = "Patient Type",
    y = "Distribution of Study Participants"
  ) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "bottom")

bcdata_graphs
```

<img src="ML_HW1_jf3354_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

We see that in the underweight category, there is only one breast cancer
patient and no healthy controls. For the Normal Weight, Overweight, and
Obesity Class I categories, the breast cancer patients surpass the
healthy controls. In the Obesity Class II group, there are more health
controls in comparison to the breast cancer patients. There were no
participants in the severly underweight and obesity class III group
which was reflected in the graphs.

To Do: make comments on the graph and message professor asking if she
wants the number of patients and controls are within each bmi catgory

## QUESTION 4

## QUESTION 5
