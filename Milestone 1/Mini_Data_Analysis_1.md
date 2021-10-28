Milestone 1
================
Christina Sun
10/5/2021

``` r
library(ggplot2)
library(datateachr)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v tibble  3.1.5     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1
    ## v purrr   0.3.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

## Task 1: Choose your favorite dataset (10 points)

**1.1 Choose 4 datasets:**

1: cancer_sample 2: apt_buildings 3: steam_games 4: vancouver_trees

**1.2 Explore the datasets:**

In this section we explore each of the 4 chosen datasets by printing out
their head of data including names of columns and the first 6 rows, and
their class types and dimensions.

1.  cancer_sample:

``` r
# Observations: All numerical except for "diagnosis"; Special tibble data frame
head(cancer_sample)
```

    ## # A tibble: 6 x 32
    ##         ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ##      <dbl> <chr>           <dbl>        <dbl>          <dbl>     <dbl>
    ## 1   842302 M                18.0         10.4          123.      1001 
    ## 2   842517 M                20.6         17.8          133.      1326 
    ## 3 84300903 M                19.7         21.2          130       1203 
    ## 4 84348301 M                11.4         20.4           77.6      386.
    ## 5 84358402 M                20.3         14.3          135.      1297 
    ## 6   843786 M                12.4         15.7           82.6      477.
    ## # ... with 26 more variables: smoothness_mean <dbl>, compactness_mean <dbl>,
    ## #   concavity_mean <dbl>, concave_points_mean <dbl>, symmetry_mean <dbl>,
    ## #   fractal_dimension_mean <dbl>, radius_se <dbl>, texture_se <dbl>,
    ## #   perimeter_se <dbl>, area_se <dbl>, smoothness_se <dbl>,
    ## #   compactness_se <dbl>, concavity_se <dbl>, concave_points_se <dbl>,
    ## #   symmetry_se <dbl>, fractal_dimension_se <dbl>, radius_worst <dbl>,
    ## #   texture_worst <dbl>, perimeter_worst <dbl>, area_worst <dbl>, ...

``` r
class(cancer_sample)
```

    ## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"

``` r
dim(cancer_sample)
```

    ## [1] 569  32

2: apt_buildings:

``` r
# Observations: Mostly categorical and binary
head(apt_buildings) 
```

    ## # A tibble: 6 x 37
    ##      id air_conditioning amenities   balconies barrier_free_acc~ bike_parking   
    ##   <dbl> <chr>            <chr>       <chr>     <chr>             <chr>          
    ## 1 10359 NONE             Outdoor re~ YES       YES               0 indoor parki~
    ## 2 10360 NONE             Outdoor po~ YES       NO                0 indoor parki~
    ## 3 10361 NONE             <NA>        YES       NO                Not Available  
    ## 4 10362 NONE             <NA>        YES       YES               Not Available  
    ## 5 10363 NONE             <NA>        NO        NO                12 indoor park~
    ## 6 10364 NONE             <NA>        NO        NO                Not Available  
    ## # ... with 31 more variables: exterior_fire_escape <chr>, fire_alarm <chr>,
    ## #   garbage_chutes <chr>, heating_type <chr>, intercom <chr>,
    ## #   laundry_room <chr>, locker_or_storage_room <chr>, no_of_elevators <dbl>,
    ## #   parking_type <chr>, pets_allowed <chr>, prop_management_company_name <chr>,
    ## #   property_type <chr>, rsn <dbl>, separate_gas_meters <chr>,
    ## #   separate_hydro_meters <chr>, separate_water_meters <chr>,
    ## #   site_address <chr>, sprinkler_system <chr>, visitor_parking <chr>, ...

``` r
class(apt_buildings)
```

    ## [1] "tbl_df"     "tbl"        "data.frame"

``` r
dim(apt_buildings)
```

    ## [1] 3455   37

3: steam_games:

``` r
# Observations: Special tibble data frame
head(steam_games) 
```

    ## # A tibble: 6 x 21
    ##      id url    types  name  desc_snippet recent_reviews all_reviews release_date
    ##   <dbl> <chr>  <chr>  <chr> <chr>        <chr>          <chr>       <chr>       
    ## 1     1 https~ app    DOOM  Now include~ Very Positive~ Very Posit~ May 12, 2016
    ## 2     2 https~ app    PLAY~ PLAYERUNKNO~ Mixed,(6,214)~ Mixed,(836~ Dec 21, 2017
    ## 3     3 https~ app    BATT~ Take comman~ Mixed,(166),-~ Mostly Pos~ Apr 24, 2018
    ## 4     4 https~ app    DayZ  The post-so~ Mixed,(932),-~ Mixed,(167~ Dec 13, 2018
    ## 5     5 https~ app    EVE ~ EVE Online ~ Mixed,(287),-~ Mostly Pos~ May 6, 2003 
    ## 6     6 https~ bundle Gran~ Grand Theft~ NaN            NaN         NaN         
    ## # ... with 13 more variables: developer <chr>, publisher <chr>,
    ## #   popular_tags <chr>, game_details <chr>, languages <chr>,
    ## #   achievements <dbl>, genre <chr>, game_description <chr>,
    ## #   mature_content <chr>, minimum_requirements <chr>,
    ## #   recommended_requirements <chr>, original_price <dbl>, discount_price <dbl>

``` r
class(steam_games)
```

    ## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"

``` r
dim(steam_games)
```

    ## [1] 40833    21

4: vancouver_trees:

``` r
# Observations: Categorical, binary and numerical
head(vancouver_trees) 
```

    ## # A tibble: 6 x 20
    ##   tree_id civic_number std_street genus_name species_name cultivar_name  
    ##     <dbl>        <dbl> <chr>      <chr>      <chr>        <chr>          
    ## 1  149556          494 W 58TH AV  ULMUS      AMERICANA    BRANDON        
    ## 2  149563          450 W 58TH AV  ZELKOVA    SERRATA      <NA>           
    ## 3  149579         4994 WINDSOR ST STYRAX     JAPONICA     <NA>           
    ## 4  149590          858 E 39TH AV  FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ## 5  149604         5032 WINDSOR ST ACER       CAMPESTRE    <NA>           
    ## 6  149616          585 W 61ST AV  PYRUS      CALLERYANA   CHANTICLEER    
    ## # ... with 14 more variables: common_name <chr>, assigned <chr>,
    ## #   root_barrier <chr>, plant_area <chr>, on_street_block <dbl>,
    ## #   on_street <chr>, neighbourhood_name <chr>, street_side_name <chr>,
    ## #   height_range_id <dbl>, diameter <dbl>, curb <chr>, date_planted <date>,
    ## #   longitude <dbl>, latitude <dbl>

``` r
class(vancouver_trees)
```

    ## [1] "tbl_df"     "tbl"        "data.frame"

``` r
dim(vancouver_trees)
```

    ## [1] 146611     20

**1.3 Narrow down to 2 datasets:**

After observing the datasets, we narrowed down to these two: 1:
apt_buildings 2: vancouver_trees

The logistics behind taking out the other two datasets are as follows:
First, the datasets “cancer_sample” and “steam_games” are both of the
class type (“spec_tbl_df”). In the dataset “steam_games” we observed
special data columns such as *complicated sentences* and *URLs*, which
will possibly make data analysis more challenging and thus not what we
want. Second, the dataset “cancer_sample” contains almost only numerical
data. However, data of multiple types will be more valuable for us to
practice data wrangling, such as the data of categorical, binary and
numerical types in “vancouver_trees”.

**1.4 Final decision on dataset of interest**

We have chosen the dataset “vancouver_trees” to investigate. We are
interested for two reasons: 1. As mentioned, this dataset has various
data types of categorical, binary and numerical, which will make our
practice of data wrangling more thorough. 2. I am interested in
potentially investigate the spread of tree species among different
regions and roads of Vancouver, and possibly form a comprehensive graph.
Another possible direction is the relationship between heights,
diameters and date planted of individual trees and how trees are spread
within different regions.

## Task 2: Exploring your dataset (15 points)

**Exercise 1: Plot Density of “diameter”**

Our dataset has a variety of tree diameters ranging from 0 to 435. We
would like to explore how diameter is distributed among all tree samples
by plotting the density of diameter. We also distinguish the density
curve by “on curb” to investigate whether trees sitting on curb have
significantly small diameter or not.

Observing the original density plot, there are several data points with
extremely large diameters that reduced visibility. After removing
extremely large diameters, we observed that the distribution of tree
diameters are centralized around 5, with a big tail extending to around
40. And the distribution are very similar for both trees on curb or not
on curb, meaning that there is no obvious relationship between
“diameter” and “on curb”.

``` r
# Plot the density of diameter and distinguish by curb
ggplot(vancouver_trees, aes(x = diameter)) + geom_density(aes(fill = curb),alpha = 0.3)
```

![](Mini_Data_Analysis_1_files/figure-gfm/Plot%20Density-1.png)<!-- -->

``` r
# Observing max value of diameter
max(select(vancouver_trees,diameter))
```

    ## [1] 435

``` r
large_diameter_removed <- filter(vancouver_trees, diameter < 50)
# Re-plot the density after removing outliers 
ggplot(large_diameter_removed, aes(x = diameter)) + geom_density(aes(fill = curb),alpha = 0.3)
```

![](Mini_Data_Analysis_1_files/figure-gfm/Plot%20Density-2.png)<!-- -->

**Exercise 2: Plot Variable Relationship between “diameter” and
“date_planted”**

We then make a point plot of “date_planted” v.s. “diameter” to find out
if the diameter of the trees have increased over time. We observed that
the range of diameter shows a decline as date_planted increases, meaning
that the older trees tend to have larger diameters, which proves our
prediction.

``` r
# Refine the range of diameter investigated to increase visibility
more_diameter_removed <- filter(vancouver_trees, diameter < 30)
ggplot(more_diameter_removed, aes(date_planted, diameter)) + geom_point(colour = "purple",size = 1,alpha = 0.02)
```

    ## Warning: Removed 69296 rows containing missing values (geom_point).

![](Mini_Data_Analysis_1_files/figure-gfm/Plot%20Variable%20Relationship-1.png)<!-- -->

**Exercise 3: Make a new variable “size”**

We are also interested in making a new variable “size” that is the
volume of a tree calculated by its base area multiplied by its height.
Looking at “size” gives a new perspective since trees with only “height”
or “diameter” being large not necessarily leads to an impressive size.
Again, we then took out some outliers with extremely large size to make
the plot more visible.

``` r
# Make a new variable of "size"
vancouver_trees <- mutate(vancouver_trees, size = height_range_id * pi * (diameter / 2)^2)
range(select(vancouver_trees, size))
```

    ## [1]      0.0 315695.5

``` r
# Remove extreme sizes
large_size_removed = filter(vancouver_trees, size < 7.5*10^3)
# Plot the density distribution of "size"
ggplot(large_size_removed, aes(size)) + geom_density()
```

![](Mini_Data_Analysis_1_files/figure-gfm/New%20Variable-1.png)<!-- -->

**Exercise 4: Make a new tibble with a subset of the data**

The final exercise is to make a new tibble containing only trees of
Japanese origins (i.e. trees that have common names starting with
“Japanese”). We will also take out some variables that we are not
interested in. In the following mini data analysis projects, we will use
this refined dataset to investigate specific research questions targeted
to trees of Japanese origins in Vancouver.

``` r
# Look at all common_name present in the dataset
unique(select(vancouver_trees, common_name))
```

    ## # A tibble: 634 x 1
    ##    common_name          
    ##    <chr>                
    ##  1 BRANDON ELM          
    ##  2 JAPANESE ZELKOVA     
    ##  3 JAPANESE SNOWBELL    
    ##  4 AUTUMN APPLAUSE ASH  
    ##  5 HEDGE MAPLE          
    ##  6 CHANTICLEER PEAR     
    ##  7 COLUMNAR NORWAY MAPLE
    ##  8 CRIMEAN LINDEN       
    ##  9 ROSE OF SHARON       
    ## 10 RAYWOOD ASH          
    ## # ... with 624 more rows

``` r
# Select only those starting with Japanese and put into a variable of common_name
selected_common_name = c("JAPANESE SNOWBELL", "JAPANESE DOGWOOD", "JAPANESE ZELKOVA", "JAPANESE BEECH", "JAPANESE MAPLE", "JAPANESE FLOWERING CRABAPPLE", "JAPANESE STEWARTIA", "JAPANESE STEWARTIA", "JAPANESE HORNBEAM", "JAPANESE PAGODA TREE", "JAPANESE CRYPTOMERIA", "JAPANESE WALNUT", "JAPANESE ANGELICA TREE", "JAPANESE WHITE PINE", "JAPANESE BLACK PINE", "JAPANESE CHESTNUT")
# Create a new tibble containing only Japanese originated trees and take out some irrelevant variables and extreme values
vancouver_trees %>%
  select(-civic_number, -assigned, -cultivar_name, -street_side_name, -longitude, -latitude) %>%
  filter(size < 7.5*10^3, diameter < 40) %>%
  filter(common_name %in% selected_common_name)
```

    ## # A tibble: 4,611 x 15
    ##    tree_id std_street       genus_name species_name common_name     root_barrier
    ##      <dbl> <chr>            <chr>      <chr>        <chr>           <chr>       
    ##  1  149563 W 58TH AV        ZELKOVA    SERRATA      JAPANESE ZELKO~ N           
    ##  2  149579 WINDSOR ST       STYRAX     JAPONICA     JAPANESE SNOWB~ N           
    ##  3  149647 E 16TH AV        STYRAX     JAPONICA     JAPANESE SNOWB~ N           
    ##  4  149658 WINDSOR ST       STYRAX     JAPONICA     JAPANESE SNOWB~ N           
    ##  5  155398 MCRAE AV         STYRAX     JAPONICA     JAPANESE SNOWB~ N           
    ##  6  155424 W 57TH AV        CORNUS     KOUSA        JAPANESE DOGWO~ N           
    ##  7  155489 COMMERCIAL ST    FAGUS      CRENATA      JAPANESE BEECH  N           
    ##  8  155951 W KING EDWARD AV STYRAX     JAPONICA     JAPANESE SNOWB~ N           
    ##  9  156066 W 38TH AV        ACER       PALMATUM     JAPANESE MAPLE  N           
    ## 10  156067 W 38TH AV        ACER       PALMATUM     JAPANESE MAPLE  N           
    ## # ... with 4,601 more rows, and 9 more variables: plant_area <chr>,
    ## #   on_street_block <dbl>, on_street <chr>, neighbourhood_name <chr>,
    ## #   height_range_id <dbl>, diameter <dbl>, curb <chr>, date_planted <date>,
    ## #   size <dbl>

## Task 3: Research Questions

1.  For all trees that have common names starting with “Japanese”, how
    are they spread across the city of Vancouver? Do they gather in
    groups by different species? Were they only planted in a small
    subarea of Vancouver?
2.  For these “Japanese” trees of different species, how are their size
    related to their species? Which species have a tall and slim figure,
    and which have a short and round shape?
3.  How are trees of different height spread across different areas
    within Vancouver? Do they follow any patterns (e.g. similar height
    on the same street)?
4.  If investigating times of falling leaves for different tree species,
    how will it affect each areas of Vancouver with different color of
    trees, amount of tree shadow, etc?
