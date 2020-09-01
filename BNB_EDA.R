# Libraries utilised
library(shinycssloaders)
library(shiny)
library(plyr)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(leaflet)
library(tidytext)
library(stringr)
library(RColorBrewer)
library(SnowballC)
library(wordcloud)
library(reticulate)
library(sunburstR)
library(networkD3)
library(ggmap)
library(shinythemes)
library(textdata)
library(lubridate)
library(scales)
library(shinyalert)
library(plotly)
#############################################################################
#read dataset
listings <- read.csv("listings.csv")
reviews <- read.csv("reviews.csv")
animations <- read.csv("animation.csv")
#############################################################################
#Data preparation Block

#Introductory Page
#############################################################################
#Airbnb Market in Boston
df_loc <- listings %>% 
  select(neighbourhood, latitude, longitude) %>% 
  group_by(neighbourhood) %>% 
  slice(1) %>% 
  ungroup()

df_tot <- listings %>% 
  group_by(neighbourhood) %>% 
  dplyr::summarise(Count = n(),
                   Share = Count)

df_sum_loc <- df_tot %>% 
  left_join(df_loc, by ="neighbourhood")


# Customize Marker Colors
get_color <- function(df_sum_loc) {
  sapply(df_sum_loc$Count, function(Count) {
    if(Count <= 100) {
      "red"
    } else if(Count <= 250 & Count > 100) {
      "orange"
    } else if(Count >= 250 & Count < 350) {
      "blue"
    } else {
      "green"
    } })
}

# Customize icon colors
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = get_color(df_sum_loc)
)

# Create bin formats
bins <- c(0, 100,250,350, 500)
pal <- colorBin(c("red", "orange", "blue", "green"), domain = df_sum_loc$Count, bins = bins)
#############################################################################
#Average Price Distribution
listings$price <- as.numeric(listings$price)
#############################################################################
#Instant Booking & Cancellation Policy
listings$cancellation_policy <- as.character(listings$cancellation_policy)
listings$room_type <- as.character(listings$room_type)
listings$instant_bookable <- as.character(listings$instant_bookable)
listings$host_is_superhost <- as.character(listings$host_is_superhost)
filteredColumns <- listings %>% select(instant_bookable,cancellation_policy, room_type,host_is_superhost)
filteredColumns <- filteredColumns %>% filter(filteredColumns$host_is_superhost != "")
filteredColumns$host_is_superhost <- ifelse(filteredColumns$host_is_superhost=='f',"Host","SuperHost")
filteredColumns[filteredColumns$host_is_superhost == 'f'] <- "Host"
filteredColumns[filteredColumns$host_is_superhost == 't'] <- "SuperHost"
filteredColumnsByFreq <-  filteredColumns %>% group_by(instant_bookable,cancellation_policy, room_type,host_is_superhost) %>%
  dplyr::summarize(Freq = n())
filteredColumnsByFreq[filteredColumnsByFreq=='f']<- "False"
filteredColumnsByFreq[filteredColumnsByFreq=='t']<- "True"
filteredColumnsByFreq[filteredColumnsByFreq=='strict_14_with_grace_period'] <- "Strict With Grace Period"
filteredColumnsByFreq[filteredColumnsByFreq=='flexible']<- "Flexible"
filteredColumnsByFreq[filteredColumnsByFreq=='moderate']<- "Moderate"

links0 <- data.frame(source = c((filteredColumnsByFreq$instant_bookable)),
                     target = c((filteredColumnsByFreq$cancellation_policy)),
                     value = c((filteredColumnsByFreq$Freq)))
links1 <- data.frame(source = c((filteredColumnsByFreq$instant_bookable)),
                     target = c((filteredColumnsByFreq$room_type)),
                     value = c((filteredColumnsByFreq$Freq)))
links2 <- data.frame(source = c((filteredColumnsByFreq$cancellation_policy)),
                     target = c((filteredColumnsByFreq$room_type)),
                     value = c((filteredColumnsByFreq$Freq)))
links3 <- data.frame(source = c((filteredColumnsByFreq$host_is_superhost)),
                     target = c((filteredColumnsByFreq$room_type)),
                     value = c((filteredColumnsByFreq$Freq))) 
links4 <- data.frame(source = c((filteredColumnsByFreq$host_is_superhost)),
                     target = c((filteredColumnsByFreq$cancellation_policy)),
                     value = c((filteredColumnsByFreq$Freq))) 

nodes0 <- data.frame(name=c(as.character(links0$source),as.character(links0$target)) %>% unique())
links0$IDsource <- match(links0$source, nodes0$name)-1
links0$IDtarget <- match(links0$target, nodes0$name)-1

nodes1 <- data.frame(name=c(as.character(links1$source),as.character(links1$target)) %>% unique())
links1$IDsource <- match(links1$source, nodes1$name)-1
links1$IDtarget <- match(links1$target, nodes1$name)-1

nodes2 <- data.frame(name=c(as.character(links2$source),as.character(links2$target)) %>% unique())
links2$IDsource <- match(links2$source, nodes2$name)-1
links2$IDtarget <- match(links2$target, nodes2$name)-1

nodes3 <- data.frame(name=c(as.character(links3$source),as.character(links3$target)) %>% unique())
links3$IDsource <- match(links3$source, nodes3$name)-1
links3$IDtarget <- match(links3$target, nodes3$name)-1

nodes4 <- data.frame(name=c(as.character(links4$source),as.character(links4$target)) %>% unique())
links4$IDsource <- match(links4$source, nodes4$name)-1
links4$IDtarget <- match(links4$target, nodes4$name)-1


my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E",
"group_F", "group_G", "group_H"]) .range(["blue", "green" , "red", "yellow", "purple", "green", "red",
"purple"])'

p0 <- sankeyNetwork(Links = links0, Nodes = nodes0, Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", colourScale = my_color)

p1 <- sankeyNetwork(Links = links1, Nodes = nodes1, Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", colourScale = my_color)

p2 <- sankeyNetwork(Links = links2, Nodes = nodes2, Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", colourScale = my_color)
p3 <- sankeyNetwork(Links = links3, Nodes = nodes3, Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", colourScale = my_color)
p4 <- sankeyNetwork(Links = links4, Nodes = nodes4, Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", colourScale = my_color)

############Donut- instant_booking##############################################
#Donut-instant_bookable
donut_0 <- plyr::count(listings$instant_bookable)
# Compute percentages
donut_0$fraction = donut_0$freq / sum(donut_0$freq)
# Compute the cumulative percentages (top of each rectangle)
donut_0$ymax = cumsum(donut_0$fraction)
# Compute the bottom of each rectangle
donut_0$ymin = c(0, head(donut_0$ymax, n=-1))
# Compute label position
donut_0$labelPosition <- (donut_0$ymax + donut_0$ymin) / 2
# Compute a good label
donut_0$label <- paste0(donut_0$x, "\n value: ", donut_0$freq)

donut_1 <- plyr::count(listings$instant_bookable)
# Compute percentages
donut_1$fraction = donut_1$freq / sum(donut_1$freq)
# Compute the cumulative percentages (top of each rectangle)
donut_1$ymax = cumsum(donut_1$fraction)
# Compute the bottom of each rectangle
donut_1$ymin = c(0, head(donut_1$ymax, n=-1))
# Compute label position
donut_1$labelPosition <- (donut_1$ymax + donut_1$ymin) / 2
# Compute a good label
donut_1$label <- paste0(donut_1$x, "\n value: ", donut_1$freq)

#Donut-cancellation policy
donut_2 <- plyr::count(listings$cancellation_policy)
# Compute percentages
donut_2$fraction = donut_2$freq / sum(donut_2$freq)
# Compute the cumulative percentages (top of each rectangle)
donut_2$ymax = cumsum(donut_2$fraction)
# Compute the bottom of each rectangle
donut_2$ymin = c(0, head(donut_2$ymax, n=-1))
# Compute label position
donut_2$labelPosition <- (donut_2$ymax + donut_2$ymin) / 2
# Compute a good label
donut_2$label <- paste0(donut_2$x, "\n value: ", donut_2$freq)

#Donut-Host is Superhost
donut_3 <- listings %>% filter(listings$host_is_superhost != "")
donut_3$host_is_superhost <- ifelse(donut_3$host_is_superhost=='f',"Host","SuperHost")
donut_3 <- plyr::count(donut_3$host_is_superhost)
# Compute percentages
donut_3$fraction = donut_3$freq / sum(donut_3$freq)
# Compute the cumulative percentages (top of each rectangle)
donut_3$ymax = cumsum(donut_3$fraction)
# Compute the bottom of each rectangle
donut_3$ymin = c(0, head(donut_3$ymax, n=-1))
# Compute label position
donut_3$labelPosition <- (donut_3$ymax + donut_3$ymin) / 2
# Compute a good label
donut_3$label <- paste0(donut_3$x, "\n value: ", donut_3$freq)

donut_4 <- listings %>% filter(listings$host_is_superhost != "")
donut_4$host_is_superhost <- ifelse(donut_4$host_is_superhost=='f',"Host","SuperHost")
donut_4 <- plyr::count(donut_4$host_is_superhost)
# Compute percentages
donut_4$fraction = donut_4$freq / sum(donut_4$freq)
# Compute the cumulative percentages (top of each rectangle)
donut_4$ymax = cumsum(donut_4$fraction)
# Compute the bottom of each rectangle
donut_4$ymin = c(0, head(donut_4$ymax, n=-1))
# Compute label position
donut_4$labelPosition <- (donut_4$ymax + donut_4$ymin) / 2
# Compute a good label
donut_4$label <- paste0(donut_4$x, "\n value: ", donut_4$freq)

###################################################################################
#sunburst
sub1 <- listings %>% select(host_since,host_neighbourhood,host_listings_count)

# Reformat data for the sunburstR package
data <- sub1 %>%
  dplyr::mutate(path = paste(host_since,host_neighbourhood,host_listings_count,sep = "-")) %>%
  dplyr::select(path, host_listings_count)

# Plot
p <- sunburst(data, legend = FALSE)
############################################################################
#wordcloud
listings <- arrange(listings,listings$id)

reviews <- reviews %>% left_join(listings, by=c("listing_id"="id"))
reviews$comments <- as.character(reviews$comments)

neighbourhood_words <- reviews %>% select(comments, neighbourhood) %>% unnest_tokens(word, comments) %>% filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))
my_lexicon <- lexicon_nrc()
nrc <- my_lexicon %>% select(word, sentiment)
prop_tot_words <- neighbourhood_words %>% group_by(neighbourhood) %>% dplyr::mutate(total_words = n()) %>% ungroup() %>% distinct(neighbourhood, total_words) %>%  arrange(desc(total_words))
listings_words <- reviews %>%
  select(id, comments) %>%
  unnest_tokens(word, comments) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

cloud <- as.data.frame(listings_words %>% 
                         group_by(word) %>%
                         dplyr::summarise(no_rows = length(word)))
words <- reviews %>%
  select(c("reviewer_id","review_scores_rating","comments")) %>%
  unnest_tokens(word, comments) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

afinn <- get_sentiments("afinn") %>% dplyr::mutate(word = wordStem(word))
reviews.afinn <- words %>%
  inner_join(afinn, by = "word")

#mixed reviews
word_summary <- reviews.afinn %>%
  group_by(word) %>%
  dplyr::summarise(mean_rating = mean(review_scores_rating), score = max(value), count_word = n()) %>%
  arrange(desc(count_word))
#good reviews
good <- reviews.afinn %>%
  group_by(word) %>%
  dplyr::summarise(mean_rating = mean(review_scores_rating), score = max(value), count_word = n()) %>%
  filter(mean_rating > mean(mean_rating)) %>%
  arrange(desc(mean_rating))
#bad reviews
bad <- reviews.afinn %>%
  group_by(word) %>%
  dplyr::summarise(mean_rating = mean(review_scores_rating), score = max(value), count_word = n()) %>%
  filter(mean_rating < mean(mean_rating)) %>%
  arrange(mean_rating)
##############################################################################
#animation
#Creating a map object
boston_map <- get_map(c(left = -71.193799, bottom = 42.22, right = -70.985746, top = 42.43), maptype = "OpenStreetMap")
ggmap(boston_map)

#Getting the data in the right form
list_Animation <- animations
list_Animation$host_since <- as.Date(list_Animation$host_since)
list_Animation<- list_Animation%>% 
  dplyr::mutate(join_year = year(host_since), join_month = month(host_since), join_date = as.Date(paste(join_month, 1, join_year, sep = "/"), '%m/%d/%Y'))
listing_smry <- list_Animation %>% 
  dplyr::count(join_year, join_month) %>% ungroup() %>%
  arrange(join_year, join_month) %>%
  dplyr::mutate(cumm_n = cumsum(n))
listing_smry <- listing_smry[complete.cases(listing_smry), ]
listing_smry <- inner_join(listing_smry, select(list_Animation, zipcode, latitude, longitude, join_year, join_month, join_date), by = c("join_year" = "join_year", "join_month" = "join_month"))

#this function has three arguments. 
#' df: a dataframe used for plotting
#' plotdate: date used for splitting the data frame into before and after
#' mapid: a number for naming the final map file
my_zip_plot <- function(df, plotdate, mapid){
  # create the background map. using the darken argument to make the map filled with black color.
  g <- ggmap(boston_map, darken = c("0.8", "black")) 
  # split the data frame for all listings before a plot date i.e. a month
  old_df <- filter(df, join_date < plotdate)
  # split the data frame for all listings for the plot date i.e. during a month
  new_df <- filter(df, join_date == plotdate)
  # plot all the listings before the current month. Make all the older listing locations as shown in circles smaller
  g <- g + geom_point(data = old_df, aes(x = longitude, y = latitude), size = 2, color = "maroon", alpha = 0.2)
  #plot all the listings during the current month. Make all the newer listing locations as shown in circles bigger to get the "pop" effect
  g <- g + geom_point(data = new_df, aes(x = longitude, y = latitude), size = 5, color = "maroon", alpha = 0.2)
  # remove axis marks, labels, and titles
  g <- g + theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), plot.title = element_blank())  
  # place the label for year 
  g <- g + ggplot2::annotate("text", x = -71.19, y = 42.41, label = "YEAR:", color = "white", size = rel(5), hjust = 0)
  # place the value of for year 
  g <- g + ggplot2::annotate("text", x = -71.19, y = 42.399, label = unique(new_df$join_year), color = "white", size = rel(6), fontface = 2, hjust = 0)
  # place the label for listings opened  
  g <- g + ggplot2::annotate("text", x = -71.19, y = 42.385, label = "LISTING COUNT:", color = "white", size = rel(5), hjust = 0)
  # place cumulative listings openings
  g <- g + ggplot2::annotate("text", x = -71.19, y = 42.376, label = comma(unique(new_df$cumm_n)), color = "white", size = rel(6), fontface = 2, hjust = 0)
  # generate the file name for the map. Using str_pad to make the filename same length and prefixed with zeroes. 
  # create a maps directory inside the directory of this script.
  filename <- paste0("www/img_" , str_pad(mapid, 7, pad = "0"),  ".png")
  #this saves the images created.
  ggsave(filename = filename, plot = g, width = 13, height = 7, dpi = 150, device = "png")
}
# Create a folder called 'map' in your working directory in prior to running the below code.
listing_smry  %>%  
  dplyr:: mutate(mapid = group_indices_(listing_smry, .dots = 'join_date')) %>% 
  group_by(join_date) %>% 
  do(pl = my_zip_plot(listing_smry, unique(.$join_date), unique(.$mapid)))
makemovie_cmd <- paste0("ffmpeg -framerate 5 -y -pattern_type glob  -i '", paste0(getwd(), "/www/"), "*.png'", " -c:v libx264 -pix_fmt yuv420p '", paste0(getwd(), "/www/"), "movie.mp4'")
system(makemovie_cmd)

#################################################################################
#UI
#################################################################################
ui <-
  fluidPage(useShinyalert(),theme = shinytheme("cerulean"),navbarPage(
    "Airbnb Rental Analysis: Boston",
    inverse = TRUE,
    collapsible = TRUE,
    #ui side functions
    tabPanel(
      strong("Places to Stay"),
      titlePanel(strong("Airbnb: A Community Built on Sharing")),
      p(strong("Boston"),"is one of the most expensive cities to live in",strong("United States"),". It is also one of the most densely populated cities in the world as it is the", strong("collegiate mecca"), "citing the", strong("highest number of schools per capita"), "for thriving scientific researches which contributes 20% of Student Population to overall population of the Boston globe. Boston is the global pioneer in", strong("Innovation & Entrepreneurship"),"making it one of the",strong("most overlooked place"),"for",strong("employment oppurtunities."),"Also, it is a",strong("year-round destination with a 4-season climate"),"& is the ",strong("most historic cities"),"in the States which attracts a lot of",strong("tourists.")
      ),
      p(
        "In a city with so much to do & high living standards, the travel & stay experience for international students,travel bloggers, Entrepreneurs  should be comfortable, secured, customisable, affordable, and to make that possible ,stepped in an alternative",strong("Airbnb"),"becoming an integral part of the", strong("travel community"), "benefiting travellers by",strong("making travel more affordable."),strong ("Since 2008, guests and hosts"),"have used Airbnb to travel in a more",strong("unique, personalized way"),".Lets take you through some interesting facts showing Utilisation of BNB's in personalised as well as affordable way without compromissing on the quality of experience contributing to the overall expansion & success of the Organisation.",
        align = "justify"),
      hr(),
      h5(
        "Click on radio buttons to view maps, illustrating the,",strong("Count of various Listings in each Neighbourhood"), "&",strong("Detailed information of each listing"),"in",strong("Boston,Massachusetts")
      ),
      h5(
        "Low: Hover on location icons to get Neighbourhood Names  & Click them to get count of listings in there"
      ),
      h5(
        "High: Click on clusters until you find location icon & click on any location icon to fetch detailed information of that listing"
      ),
      br(),
      
      fluidRow(style = "border-style:solid;border-color:black",
        br(),
        column(2,wellPanel(radioButtons(
          "radiomap",
          label = "Level of Granularity",
          c("Low" = 0,
            "High" = 1),selected = 0
        ))),
        column(7,withSpinner(leafletOutput("BostonAirbnbListings"))),
        br(),
        column(
          3,
          p(
            "As observed from the first map, Maximum number of Airbnb Rentals are listed in neighbourhoods", strong("Dorchester & Allston-Brighton"), "& the count being", strong("430 & 397"), "respectively which has been summarised for all longitudinal & latitudinal coordinates of the listings in that particular neighbourhood. ",style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px",
            align = "justify"
          ),
          p(
            "The CartoDB map with the higher level of granularity has all the listings in Boston plotted in a clustered manner, utilising the exact latitude & longitude coordinates of each listing providing the exact loaction & the detailed information of the listing including",strong("Name of listing, Host Name, Price, Room & Property Type"),style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"
          )
        )
      )),
    

    tabPanel("Pricing",
               titlePanel(strong("Price Distribution of Rental Types across Neighbourhoods")),
               p("Now that you are familiarised with the Neighbourhoods in the city, lets take you across the types of rentals in each neighbourhood comparing their price & establishing relationship between", strong("Property Types, Average Price & Neighbourhood"),"so that you are aware of the cheapest & expensive rental types along with the top rental types in there, in terms of count. The factors taken into consideration for rentals utilisation have been emphasised depending on the",strong("Affordibility, Comfort & Location Preference.")),
               hr(),
               fluidRow(style = "border-style:solid;border-color:black", br(),column(2,wellPanel(selectInput(
                 "Borough",
                 label = "Neighbourhood",
                 c("East Boston" = "East Boston",
                   "West Roxbury" = "West Roxbury",
                   "Downtown" = "Downtown",
                   "Back Bay" = "Back Bay",
                   "North End" = "North End",
                   "Beacon Hill" = "Beacon Hill",
                   "Dorchester" = "Dorchester",
                   "South End" = "South End",
                   "Charlestown" = "Charlestown",
                   "Jamaica Plain" = "Jamaica Plain",
                   "Allston-Brighton" = "Allston-Brighton",
                   "South Boston" = "South Boston",
                   "Fenway/Kenmore" = "Fenway/Kenmore",
                   "West Roxbury" = "West Roxbury",
                   "Roslindale" = "Roslindale",
                   "Mission Hill" = "Mission Hill",
                   "Roslindale" = "Roslindale",
                   "West End" = "West End",
                   "Chinatown" = "Chinatown",
                   "Mattapan" = "Mattapan",
                   "Hyde Park" = "Hyde Park",
                   "Brookline" = "Brookline",
                   "Somerville" = "Somerville",
                   "Cambridge" = "Cambridge",
                   "Chelsea" = "Chelsea",
                   "Everett" = "Everett",
                   "Leather District" = "Leather District"),selected = "Jamaica Plain"),br(),wellPanel(p("Choose Neighbourhood from the dropdown")))),
                 column(
                  10,
                   fluidRow(
                     p(
                       "Below Box Plots show the Average Price Distribution of each Property Type in the Neighbourhood.From the below observations & comparisons the Neighbourhoods with expensive rentals are West End,Back Bay,Fenway & South Boston as rental type of higher prices like Guest suites, Lofts, Serviced Apartments, House Townhouse are majorly in these neighbourhods. For a Luxurious stay experience, these neighbourhoods are highly preferrable. But overall distrubution tells for an expensive city like Boston, renting a Hotel costs a Fortune, wherein Airbnb rentals are quite economic & reasonable",style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"
                       ,
                       align = "justify"
                     ),
                     h5(
                       strong("Hover over the plots to see the maximum, quartiles,median & minimum price of rental type for a neighbourhood.")
                     )
                   ),
                   br(),
                   fluidRow(withSpinner(plotlyOutput("boxplotavg")))
                 ),
                 column(2),
                 column(10,fluidRow(
                   p(
                     "There are almost 20 types of properties listed overall in the city. Not all neighbourhood have all types of property listed. Below chart illustrates count of all types of rentals available in a neighbourhood. You will observe that mostly the top 4 property types listed are", strong("Apartment, Condominium, House & Townhouse"),"across all the neighbourhoods.",style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px",
                     align = "justify"
                   ),
                   h5(
                     "Hover on bars to see count of rental type"
                   ),
                 ),
                 
                 fluidRow(withSpinner(plotlyOutput("barchartprop"))))
               )
      ),
      tabPanel(
        "Instant Book & Cancellation Policies",
        titlePanel(strong("Flexibile Policies with repect to Rentals with HOST/SUPER HOST")),
        p(
          "Users choose a rental with flexible policies to be able to book instantly to reduce hassle of booking process or to incur no extra cost if they had to cancel for any reason. On the other hand, Airbnb HOSTS & SUPERHOSTS select a cancellation policy or instant book policy based on their own level of comfort and risk tolerance. There’s something more to choosing a cancellation policy/Instant Book apart from comfort & risk optimisation for them.Through this analysis we try to show you the relationship between the parameters INSTANT BOOKING, CANCELLATION POLICY, ROOM TYPE & TYPE OF HOST as these features are of great importance to both customers and owners at the time of Renting an Airbnb for an overall revenue optimisation for owners as well as improvisation of user experience.",
          align = "justify"
        ),
        fluidRow(style = "border-style:solid;border-color:black",br(),column(3,wellPanel(radioButtons("sankeyinput","Relationship between",
                                       c("Instant Booking - Cancellation Policy" = 0,
                                         "Instant Booking - Room Type" = 1,
                                         "Cancellation Policy - Room Type" = 2,
                                         "Type of Host - Room Type" = 3,
                                         "Type of Host - Cancellation Policy" = 4),selected = 0),wellPanel(p("Note: Hover on the Sankey Nodes & Relations"))),h5(strong("Inference from the Relationships:")),p("As you can observe, from the relationships, 60% of listings of Airbnb has Instant booking available, out of which majority is available on entire home/apartment room types.It is also observed that strict cancellation policies are merely less for the any BNB room types increasing the preferabilty of BNB utilisation from users perspective.The number of listings listed by Host are more than super hosts, & superhosts have mostly listed entire homes/apratments.No unique patterns observed between super host and regular host for booking/cancellation policies ensuring uniformity of ploicy across all listings",style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px")),  
                 column(4,h5("Diagram illustrating the visual flow of dominant contributions of",strong("Instant Booking, Cancellation Policy, Room Type & Type of Host"),"to understand overall Rental Policy Flexibility"  ), 
                        withSpinner(sankeyNetworkOutput("sankey"))),
                 column(5, h5("Chart listing the sub categories & count of the parameters",strong("Instant Booking, Cancellation Policy, Room Type & Type of Host"),"spread over the listings",),
                        fluidRow(withSpinner(plotOutput("donuts")),
                        h5(strong("Details of the sub categories:")),
                        p(strong("INSTANT BOOKING"),"-Values for this has been demarketed as 't' or 'f' showing which listings have or do not have instant booking feature available.Instant booking feature enables good user experience by ensuring guests to no more wait for the hosts approval & instead to enter travel dates directly & proceed discussing checkin with host/superhost.", 
                          strong("CANCELLATION POLICY Options"),"-",
                          strong("Flexible:"),"Full refund if cancellation is within 24 hours
                          before check-in,",
                          strong("Moderate:"),"Full refund if cancellation is within 5 days before check-in,",
                          strong("Strict:"),"Full refund if cancellation is within 48 hours of booking,",
                          strong("Strict_14_with_grace_period:"),"Full refund if cancellation before 14 full days,",         
                          strong("Super Strict:"),"50% refund up until 30 to 60 days prior to check-in.",
                          strong("ROOM TYPE"),"-Entire home/apt, Private Room, Hotel Room, Shared Room.",
                          strong("Type of Host"),"- HOST & SUPER HOST",style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px",align = "justify"
                          )
                  
                 )
                        ))
      ),
      tabPanel(
        "User Reviews",
        titlePanel(strong("
          Performance Analysis: What does the User Experience highlight?")
        ),
        br(),
        style = "border-style:solid;border-color:black",
        p(
          " According to Online Review Survey, 94% of consumers get convinced by online reviews to avoid a business. Airbnb being one of the leading travel communities providing travellers unique experiences. So, certainly reviews are an essential part of of Airbnb experience showing a social proof to inspire the travellers confidence.",style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"),
        p("Wondering how do we get to know about user experince from the huge heap of data we have since 2008? There are almost 2 lakh reviews written by the users sharing their good & bad experiences.The below World Cloud does that for you.Wordclouds basically take a frequency count of the words whole as input, and return a beautiful display of frequently occurring words, with their size being proportional to their relative frequency. So, herein the aim is to show you the dominant themes in Reviews.The dominant the word is in word cloud, the more is its frequency of Occurence in the User Reviews displaying how these reviews considerably are contributing to Organisation's & giving it more room to improvise as well by showing up the bad experinces as well.Let the Wordcloud speak for the overall highlights of the Airbnb Experience of users in Boston.",style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px"
        ),
        hr(),
        fluidRow(column(3, 
                        wellPanel(selectInput("selection", "Review Type:",
                                    c("Mixed Reviews" = 0,
                                      "Positive Reviews" = 1,
                                      "Negative Reviews" = 2),selected = 0),
                        hr(),
                        wellPanel(sliderInput("max",
                                    "Maximum Number of Words:",
                                    min = 50,  max = 1500,  value = 100)
        ),p("Choose Review Type from the dropdown & scroll the slider to Increase or Decrease words to display"))),
        column(8,withSpinner(plotOutput("wordcloudrev",height = "650", width = "90%")))
        )),
      tabPanel(
          "Market Expansion",
          titlePanel(strong("BNB Market Expansion in Boston")),
          fluidRow(style = "border-style:solid;border-color:black",br(),column(6,p(
            "Airbnb’s first Boston’s listing was in East Boston in the year 2008, and the growth since then has been tremendous. The below graph depicts the Airbnb’s growth in Boston over the years since it has emerged. Around 400 properties were added in the first 4 years, mostly in",strong("Back Bay, Dorchester, Charlestown & Beacon Hill."),strong("Downtown"), "and", strong("Dorchester"),"have the highest Airbnb presence today. The number of listings has roughly increased each year since 2010. By the year 2014, every neighborhood of Boston had  multiple listings.",style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px",
            h5(strong("Hover on the rings to visualise the increase in number of listings in neighbourhoods every year in an hierarchical pattern.")),
            align = "justify"
          ),
          withSpinner(sunburstOutput("sunburst",height = "650", width = "100%"))
        ),
      h4(
        "Animated Video: When Airbnb Listings in a City Increased"
      ),
      h5(
        "Play the video to visualise the expansion of BNB across the Boston Globe"
      ),
      column(6,(tags$video( src="movie.mp4",type = "video/mp4", width ="650px",height ="450x",controls = "controls")),
             br(),
             br(),
             p("Airbnb has revolutionised the travel industry. They emerged in 2008, & since then have been emulating on every level, may that be from business model, map & search results, aproach to basically everything they do. They have been around for more than a decade now & are still growing at the rate that most business can only dream of. Airbnb as an organisation keeps growing despite of the fact every one knows about it, as it ensures that people who have stayed with them do it again, by diversifying their product range from others, by making it really reasonable in metropolitan & expensive cities like Boston, by ensuring user friendly policies to make their experience homely & memorable. They have been able to make this success story as they are committed to understand the need of the Travellers as well as the Owners & this will let them grow over coming years.",style="text-align:justify;color:black;background-color:lightblue;padding:15px;border-radius:10px",align = "justify"),
        tags$i(
        "Disclaimer: There is no association with Airbnb or its Founders. This analysis has been completed using publicly available data over internet."
      )))
      
    
    )))
#################################################################################
#SERVER
#################################################################################



server <- function(input, output) {
  shinyalert(
    title = "Looking for best bang to stay while exploring the beautiful city of Boston for your buck around the city?? Hop in and find out!!",
    text = "",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "success",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "Continue Reading",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "https://ewscripps.brightspotcdn.com/dims4/default/4a23d5e/2147483647/strip/true/crop/1676x943+105+0/resize/1280x720!/quality/90/?url=http%3A%2F%2Fewscripps-brightspot.s3.amazonaws.com%2F5b%2F41%2F41d5f6c44ff4b8c9e6953163fc4b%2Fairbnb-horizontal-lockup-white-print.jpg",
    imageWidth = 100,
    imageHeight = 100,
    animation = TRUE
  )
###################################### TAB 1 ################################### 
  output$BostonAirbnbListings <- renderLeaflet({
    if (input$radiomap == 0){
      leaflet(data = df_sum_loc) %>% 
        setView(lat = 42.35210, lng =  -71.06168, zoom = 12, options = 
                  leafletOptions(minZoom = 14, dragging = FALSE)) %>% 
        addTiles() %>% 
        addAwesomeMarkers(lat   = ~latitude, lng = ~longitude, 
                          popup = ~paste(as.character(Share), sep =""), 
                          label = ~neighbourhood, 
                          icon  = icons) %>% 
        addLegend(
          pal = pal,
          values = ~Count,
          opacity = 1,
          title = "Number of Listings",
          position = "bottomright")}
    else {
      leaflet(listings) %>%
        addTiles() %>%
        addMarkers(~longitude, ~latitude,labelOptions = labelOptions(noHide = F),clusterOptions = markerClusterOptions(),popup = paste0("<b> Name: </b>", listings$name , "<br/><b> Host Name: </b>", listings$host_name, "<br> <b> Price: </b>", listings$price, "<br/><b> Room Type: </b>", listings$room_type, "<br/><b> Property Type: </b>", listings$property_type
        )) %>% 
        setView(-71.057083, 42.361145, zoom = 12) %>%
        addProviderTiles("CartoDB.Positron")
    }})
  
###################################### TAB 2 ###################################
  output$boxplotavg <- renderPlotly(
    ggplotly(ggplot(subset(listings,listings$neighbourhood ==input$Borough),aes(y = price)) + geom_boxplot(aes(colour = property_type )) + facet_grid(neighbourhood~property_type) + ggtitle("Property Types in Neighbourhood")+theme(plot.title = element_text(hjust = 0.5)) + labs(y="Average Price") + scale_x_continuous(breaks = 0)
  ))
  output$barchartprop <- renderPlotly(
    ggplotly(ggplot(subset(listings,listings$neighbourhood == input$Borough),aes(x =  property_type)) +
      geom_bar(aes(colour = property_type ))  +
      ggtitle("Which types of Listings are there in this Neighborhood?",subtitle = "Graph showing Count of Listing Types") +
      theme(plot.title = element_text(face = "bold",color = "#e06f69")) +
      theme(plot.subtitle = element_text(face = "bold", color = "#7db5b8")) +
      theme(plot.caption = element_text(color = "grey68")) +
      xlab("Property Type") + ylab("Count") + theme(axis.text.x = element_text(size = 10, angle = 90)))
  )
  
###################################### TAB 3 ###################################
  output$sankey <- renderSankeyNetwork(
    if (input$sankeyinput==0){
      p0
    }else if(input$sankeyinput==1){p1
    }else if(input$sankeyinput==2){p2
    }else if(input$sankeyinput==3){p3
    }
    else{p4})

  output$donuts <- renderPlot(
    if(input$sankeyinput==0){
      ggplot(donut_0, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=x)) +
        geom_rect() +
        geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) + #For lablelling
        # scale_fill_brewer(palette=4) + (If required uncomment it)
        scale_color_brewer(palette=4) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "none") +
        ggtitle("Count of Instant Bookable listings based on Cancellation Policy")+theme(plot.title = element_text(hjust = 0.5))}else if (input$sankeyinput==1){ggplot(donut_1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=x)) +
            geom_rect() +
            geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) + #For labelling
            # scale_fill_brewer(palette=4) + (If required uncomment it)
            scale_color_brewer(palette=4) +
            coord_polar(theta="y") +
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "none") +
            ggtitle("Count of Instant Bookable listings based on Room Types")+theme(plot.title = element_text(hjust = 0.5))}else if(input$sankeyinput==2){ggplot(donut_2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=x)) +
                geom_rect() +
                geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) + #For labelling
                # scale_fill_brewer(palette=4) + (If required uncomment it)
                scale_color_brewer(palette=4) +
                coord_polar(theta="y") +
                xlim(c(2, 4)) +
                theme_void() +
                theme(legend.position = "none") +
                ggtitle("Types & Count of Cancellation Policies available for Room Types ")+theme(plot.title = element_text(hjust = 0.5))}else if(input$sankeyinput==3){ggplot(donut_3, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=x)) +
                    geom_rect() +
                    geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) + #For labelling
                    # scale_fill_brewer(palette=4) + (If required uncomment it)
                    scale_color_brewer(palette=4) +
                    coord_polar(theta="y") +
                    xlim(c(2, 4)) +
                    theme_void() +
                    theme(legend.position = "none") +
                    ggtitle("Count of Host/Superhosts for Room Types available ")+theme(plot.title = element_text(hjust = 0.5))
                  
                }else{ggplot(donut_4, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=x)) +
                    geom_rect() +
                    geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) + #For labelling
                    # scale_fill_brewer(palette=4) + (If required uncomment it)
                    scale_color_brewer(palette=4) +
                    coord_polar(theta="y") +
                    xlim(c(2, 4)) +
                    theme_void() +
                    theme(legend.position = "none") +
                    ggtitle("Coun of Hosts/Superhosts based on them having various Cancellation Policies")+theme(plot.title = element_text(hjust = 0.5))})
 
##################################### TAB 4 #####################################  
  output$wordcloudrev <- renderPlot(
    if(input$selection == 1){
      wordcloud(words = cloud$word, freq = cloud$no_rows, min.freq = 1,scale =c(5,1.2),max.words=input$max, random.order=FALSE, rot.per=0.1, 
                colors=brewer.pal(8, "Dark2"))
    }else if(input$selection == 2){
      wordcloud(words = bad$word, freq = (bad$count_word), scale =c(5,1.2), max.words=mean(input$max), colors =  brewer.pal(8, "Dark2"))
    }else{
      wordcloud(words = word_summary$word, freq = word_summary$count_word, scale=c(5,1.2), max.words = mean(input$max), colors = brewer.pal(8, "Dark2"))
    }
    
  )
  
###################################### TAB 5 #################################### 
  output$sunburst<- renderSunburst(p,)
}

#################################################################################
#Run Shiny App
shinyApp(ui = ui, server = server)