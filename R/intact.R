# handling data retrieval from IntAct ------------------------------------------

# retrieve from here
# https://ftp.ebi.ac.uk/pub/databases/intact/current/psimitab/
  
# extract and create
# careful more than one file, and large - gets truncated!!

# in theory, would go with
# vroom::vroom(unz("~/Downloads/intact.zip", "intact.txt"))

# but cuts it to 995,802 × 42

# and it should be 
# 1194447 x 42
