library(tidyverse)
library(sparkline)
library(formattable)

tuesdata <- tidytuesdayR::tt_load(2020, week = 38)

kids <- tuesdata$kids

#Top 10 states with most spending per kid

top10 <- kids %>% group_by(state) %>%
  filter(year==2016) %>%
  summarise(total=round(sum(inf_adj_perchild),1)) %>%
  top_n(10,total)


#Prepare the data with sparklines

test <- kids %>% select(state,variable,year,inf_adj_perchild) %>% 
  inner_join(top10,by="state") %>%
  filter(variable %in% c("PK12ed","highered")) %>% 
  
  #Make each year one column
  pivot_wider(id_cols=c(state,variable,total),names_from=year,values_from=inf_adj_perchild) %>% 
  
  #Create the sparkline
  rowwise() %>%
  mutate(sparkline=c_across(`1997`:`2016`) %>%
           as.numeric() %>%
           sparkline(type = "line") %>%
           htmltools::as.tags() %>%
           as.character()) %>% 
  
  #Formatting for the formattable
  select(state,total,variable,`1997`,sparkline,`2016`) %>% 
  mutate(change=percent(`2016`/`1997`-1,digits=1L)) %>%
  mutate(across(c(`1997`,`2016`),~round(.,1))) %>% 
  pivot_wider(id_cols=c(state,total),names_from=variable,values_from=c(`1997`,sparkline,`2016`,change)) %>% 
  select(state,total,ends_with("12ed"),ends_with("ered")) %>%
  arrange(-total) %>% 
  
  #Make pie chart
  rowwise() %>%
  mutate(pie=c_across(c(`2016_PK12ed`,`2016_highered`)) %>%
           as.numeric() %>%
           sparkline(type="pie",sliceColors=c("#228B22","#DAA520")) %>%
           htmltools::as.tags() %>%
           as.character) %>% 
  relocate(pie,.after=total) %>% 
  
  rename(`Total spending per child 2016`=total,
    `Prim./Sec. ed.`=`1997_PK12ed`,
         `  `=sparkline_PK12ed,
         `   `=`2016_PK12ed`,
         `       `=change_PK12ed,
         `Higher ed.`=`1997_highered`,
         `     `=sparkline_highered,
         `      `=`2016_highered`,
         `        `=change_highered,
    `Education spending 2016`=pie)


#Make formatters
bold_grey <- formatter("span", style = ~ style(color = "grey", font.weight = "bold"))


out <- test %>% 
  formattable(
     align = c("l","l","l","l","r","r","r","l","r","l","r"),
     list(
       state=formatter("span",style = ~ style("font-size"="16")),
       `Total spending per child 2016`=color_bar(),
       `Prim./Sec. ed.` = formatter("span", style = ~ style(color = "grey", 
                                                            font.weight = "bold", 
                                                            "padding-left" = "80px",
                                                            "border-left" = "grey 1px solid")),
       ` ` = bold_grey,
       `   `=bold_grey,
       `       `=color_tile("#90EE90","#228B22"),
       `Higher ed.` = formatter("span", style = ~ style(color = "grey", 
                                                        font.weight = "bold", 
                                                        "padding-left" = "60px",
                                                        "border-left" = "grey 1px solid")),
       `      ` = bold_grey,
       `        `=color_tile("#FAFAD2","#DAA520")
       )
) %>% as.htmlwidget()

out$dependencies <- c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))

out

