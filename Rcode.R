# How to import the dataframe in R ----

df <- read.csv("filelocation/dataset.csv", header = TRUE, na = "999")

    # Rename the columns for ease of use
    colnames(df) <- c("INTNR", "gender", "age", "region_ita", "region_swe", 
                      "lik_ep", "lik_fl", "lik_dr", "lik_wf", "lik_ea", "lik_ta", "lik_dv", "lik_ec", "lik_cc", 
                      "dam_ep",  "dam_fl", "dam_dr", "dam_wf", "dam_ea", "dam_ta", "dam_dv", "dam_ec", "dam_cc", 
                      "dam_oth_ep", "dam_oth_fl", "dam_oth_dr", "dam_oth_wf", "dam_oth_ea", "dam_oth_ta", "dam_oth_dv", "dam_oth_ec", "dam_oth_cc", 
                      "prep_aut_ep", "prep_aut_fl", "prep_aut_dr", "prep_aut_wf", "prep_aut_ea", "prep_aut_ta", "prep_aut_dv", "prep_aut_ec", "prep_aut_cc", 
                      "prep_ep", "prep_fl", "prep_dr", "prep_wf", "prep_ea", "prep_ta", "prep_dv", "prep_ec", "prep_cc",    
                      "know_aut_ep", "know_aut_fl", "know_aut_dr", "know_aut_wf", "know_aut_ea", "know_aut_ta", "know_aut_dv", "know_aut_ec", "know_aut_cc", 
                      "know_ep", "know_fl", "know_dr", "know_wf", "know_ea", "know_ta", "know_dv", "know_ec", "know_cc",
                      "exp_ep", "exp_fl", "exp_dr", "exp_wf", "exp_ea", "exp_ta", "exp_dv", "exp_ec", "exp_cc", 
                      "edu", "income", "work", "sector", "pol", "weight", "area")

# How to create maps of the two countries indicating number of participants in each region ----

library(rnaturalearth)
library(rnaturalearthhires)
library(viridis)
library(ggplot2)

    # Retrieve the datasets with the geo coordinates ----
    
    italy <- ne_states(country = "italy", returnclass = "sf")
    sweden <- ne_states(country = "sweden", returnclass = "sf")
    
    # Then, merge the amount of respondents from the df to the datasets with the geo coordinates ----  
    
    italy$amount <- ifelse(italy$region == "Piemonte", 73,
                           ifelse(italy$region == "Valle d'Aosta", 3,
                                  ifelse(italy$region == "Liguria", 29,
                                         ifelse(italy$region == "Lombardia", 175,
                                                ifelse(italy$region == "Trentino-Alto Adige", 17,
                                                       ifelse(italy$region == "Veneto", 108,
                                                              ifelse(italy$region == "Friuli-Venezia Giulia", 18,
                                                                     ifelse(italy$region == "Emilia-Romagna", 83,
                                                                            ifelse(italy$region == "Toscana", 64,
                                                                                   ifelse(italy$region == "Umbria", 12,
                                                                                          ifelse(italy$region == "Marche", 25,
                                                                                                 ifelse(italy$region == "Lazio", NA, #1007, #
                                                                                                        ifelse(italy$region == "Abruzzo", 23,
                                                                                                               ifelse(italy$region == "Molise", 10,
                                                                                                                      ifelse(italy$region == "Campania", 127,
                                                                                                                             ifelse(italy$region == "Puglia", 83,
                                                                                                                                    ifelse(italy$region == "Basilicata", 12,
                                                                                                                                           ifelse(italy$region == "Calabria", 29,
                                                                                                                                                  ifelse(italy$region == "Sicilia", 99, 36)))))))))))))))))))
    
    sweden$amount <- ifelse(sweden$name == "Blekinge", 209,
                            ifelse(sweden$name == "Dalarna", 99,
                                   ifelse(sweden$name == "Gävleborg", 99,
                                          ifelse(sweden$name == "Gotland", 92,
                                                 ifelse(sweden$name == "Halland", 275,
                                                        ifelse(sweden$name == "Jämtland", 62,
                                                               ifelse(sweden$name == "Jönköping", 92,
                                                                      ifelse(sweden$name == "Kalmar", 92,
                                                                             ifelse(sweden$name == "Kronoberg", 92,
                                                                                    ifelse(sweden$name == "Norrbotten", 71,
                                                                                           ifelse(sweden$name == "Orebro", 244,
                                                                                                  ifelse(sweden$name == "Östergötland", 244, 
                                                                                                         ifelse(sweden$name == "Skåne", 209,
                                                                                                                ifelse(sweden$name == "Södermanland", 244,
                                                                                                                       ifelse(sweden$name == "Stockholm", NA, #1069, #
                                                                                                                              ifelse(sweden$name == "Uppsala", 244,
                                                                                                                                     ifelse(sweden$name == "Värmland", 99,
                                                                                                                                            ifelse(sweden$name == "Västerbotten", 71,
                                                                                                                                                   ifelse(sweden$name == "Västernorrland", 62, 
                                                                                                                                                          ifelse(sweden$name == "Västmanland",244, 275))))))))))))))))))))
    # Finally, map the data with ggplot ----
    
    mapIta <- ggplot(data = italy) +
                geom_sf(aes(fill = amount)) +
                scale_fill_gradientn(colours = rev(viridis(8)), 
                                     na.value = "#440154FF", 
                                     breaks = c(350, 300, 250, 200, 150, 100, 50),
                                     labels = c("350+","300", "250", "200", "150", "100", "50"),
                                     limits = c(0,350), 
                                     guide = guide_colorbar(
                                       title = "Respondents spatial distribution",
                                       direction = "horizontal",
                                       barheight = unit(2, units = "mm"),
                                       barwidth = unit(50, units = "mm"),
                                       draw.ulim = F,
                                       title.position = 'top')) +
                theme_void()
    
    
    mapSwe <- ggplot(data = sweden) +
                geom_sf(aes(fill = amount)) +
                scale_fill_gradientn(colours = rev(viridis(8)),
                                     na.value = "#440154FF", 
                                     breaks = c(350, 300, 250, 200, 150, 100, 50),
                                     labels = c("350+","300", "250", "200", "150", "100", "50"),
                                     limits = c(0,350),
                                     guide = guide_colorbar(
                                       title = "Respondents spatial distribution",
                                       direction = "horizontal",
                                       barheight = unit(2, units = "mm"),
                                       barwidth = unit(50, units = "mm"),
                                       draw.ulim = F,
                                       title.position = 'top')) +
                theme_void()
    
    
    
    
# How to create the radar charts (example for the variable "Impact") ----
library(fmsb)
library(stats)

df$area2 <- ifelse(df$area == 1, 1,
                   ifelse(df$area == 2, 1, 2)) 
df$area2 <- as.factor(df$area2) # creates a factor with 2 levels (Italy = 1, Sweden = 2)

df_dam <- df[, c(15:32, 85)] # takes only the columns needed for the chart

    # Dataframe for the variable "Impact on respondent"
df_dam_ind <- aggregate(cbind(dam_ep, dam_fl, dam_dr, dam_wf, 
                              dam_ea, dam_ta, dam_dv, dam_ec, dam_cc) ~ area2, 
                        data = df_dam, mean, na.rm = TRUE) # calculates the mean for each of the variables, per country
    
    # Dataframe for the variable "Impact on others in the country"
df_dam_oth <- aggregate(cbind(dam_oth_ep, dam_oth_fl, dam_oth_dr, dam_oth_wf,
                              dam_oth_ea, dam_oth_ta, dam_oth_dv, dam_oth_ec, dam_oth_cc) ~ area2, 
                        data = df_dam, mean, na.rm = TRUE) # calculates the mean for each of the variables, per country

colnames(df_dam_ind) <- c("area", "Epidemics", "Floods", "Drought", "Wildfires", "Earthquakes",
                          "Terror attacks", "Domestic violence", "Economic crises", "Climate Change")
colnames(df_dam_oth) <- c("area", "Epidemics", "Floods", "Drought", "Wildfires", "Earthquakes",
                          "Terror attacks", "Domestic violence", "Economic crises", "Climate Change")

    # Create dataframe for Italy --------
    df_dam_ita <- rbind(df_dam_ind[1,], df_dam_oth[1,]) # puts together the first rows of the above datasets (i.e. those referring to Italy)
    df_dam_ita <- df_dam_ita[,2:10]
    rownames(df_dam_ita) <- c("on the respondent", "on others in the country")
    colnames(df_dam_ita) <- c("Epidemics", "Floods", "Drought", "Wildfires", "Earthquakes",
                              "Terror attacks", "Domestic violence", "Economic crises", "Climate Change")
    df_dam_ita <- rbind(rep(5,9) , rep(1,9) , df_dam_ita) # adds rows for the successful creation of radarcharts
    
    
    # Create dataframe for Sweden ---------
    df_dam_swe <- rbind(df_dam_ind[2,], df_dam_oth[2,]) # puts together the second rows of the above datasets (i.e. those referring to Sweden)
    df_dam_swe <- df_dam_swe[,2:10]
    rownames(df_dam_swe) <- c("on the respondent", "on others in the country")
    colnames(df_dam_swe) <- c("Epidemics", "Floods", "Drought", "Wildfires", "Earthquakes",
                              "Terror attacks", "Domestic violence", "Economic crises", "Climate Change")
    df_dam_swe <- rbind(rep(5,9) , rep(1,9) , df_dam_swe) # adds rows for the successful creation of radarcharts
    
    
    # Create the radar charts ------------
    colors_border <- c("#b9de28ff", "#47972aff") 
    
    chart_ita <- radarchart(df_dam_ita, 
                            axistype = 1 , 
                            #customize the polygons
                            pcol = colors_border, 
                            #pfcol = , # for filling the polygons
                            pty = 32,
                            plwd = 2, 
                            plty = 1,
                            #customize the grid
                            cglcol = "grey", 
                            cglty = 1, 
                            axislabcol = "grey", 
                            caxislabels = seq(1,5,1), 
                            cglwd = 0.8,
                            #custom labels
                            vlcex = 0.9,
                            title = "Italy - Perceived impact of the following threats")
                legend(x = 1.5, y = 1, legend = c("on the respondent", "on others in the country"), 
                       bty = "n", pch = 20 , col = colors_border, text.width = 2, cex = 0.8, pt.cex = 2)    
                
    chart_swe <- radarchart(df_dam_swe, 
                            axistype = 1 , 
                            #customize the polygons
                            pcol = colors_border, 
                            #pfcol = , # for filling the polygons
                            pty = 32,
                            plwd = 2, 
                            plty = 1,
                            #customize the grid
                            cglcol = "grey", 
                            cglty = 1, 
                            axislabcol = "grey", 
                            caxislabels = seq(1,5,1), 
                            cglwd = 0.8,
                            #custom labels
                            vlcex = 0.9,
                            title = "Sweden - Perceived impact of the following threats")
                legend(x = 1.5, y = 1, legend = c("on the respondent", "on others in the country"), 
                       bty = "n", pch = 20 , col = colors_border, text.width = 2, cex = 0.8, pt.cex = 2) 
                    