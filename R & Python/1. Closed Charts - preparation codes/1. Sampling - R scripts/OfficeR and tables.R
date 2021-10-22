library(officer)
library(flextable)

df1 <- data.frame(
  Country = c("France", "England", "India", "America", "England", "India"),
  City = c("Paris", "London", "Mumbai", "Los Angeles", "Surrey", "Chennai"),
  Order_No = c("1", "2", "3", "4", "5", "6"),
  State = c("Yes", "No", "Yes", "No", "Yes", "Transit"),
  stringsAsFactors = FALSE
)

# this is a "proc freq" like ----
pf <- proc_freq(df1, "Country", "State")
pf <- fontsize(pf, size = 11, part = "all")
pf <- padding(pf, padding = 1, part = "all")
pf <- valign(pf, valign = "top", part = "all")
pf <- autofit(pf)



my_pres <- read_pptx() 
my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "heading", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = df1, location = ph_location_type(type = "body"), header = TRUE)

my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, value = "Summary", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, value = pf, 
                   location = ph_location_type(type = "body"), header = TRUE)
print(my_pres, target = "first_example.pptx")