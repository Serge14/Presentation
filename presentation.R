setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201802")

library(data.table)
library(reshape2)
library(googlesheets)

##### EXTREMELY IMPORTANT !!! ####
## File has to be pre-processed!

# Read dat file

data = fread("BFprocessed.csv", header = TRUE, stringsAsFactors = FALSE, data.table = TRUE)

# Set current month
YTD.No = 2

dataTable = function(measure, level, linesToShow, filterSegments = NULL) {
    
    # Create subset which consists only segments we'll work with
    #df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
    #          by = .(Brand, PS2, PS3, PS, Company, Ynb, Mnb, Form, Additives, PriceSegment)]
    
    df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
              by = .(Brand, PS0, PS2, PS3, PS, Company, Ynb, Mnb, PriceSegment)]
    


    if (level == "Company")  {df = data.table::dcast(df, Company~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Brand") {df = data.table::dcast(df, Brand~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Segment") {df = data.table::dcast(df, PriceSegment~Ynb+Mnb, fun = sum, value.var = measure)}
    
    nc = length(df)
    
    df$YTDLY = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No - 11):(nc - 12)]
    df$YTD = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No + 1):nc]
    df$MATLY = df[, Reduce(`+`, .SD), .SDcols = (nc - 23):(nc - 12)]
    df$MAT = df[, Reduce(`+`, .SD), .SDcols = (nc - 11):nc]
    
    df = df[, .SD, .SDcols=c(1, nc-1, nc, (nc+1):(nc+4))]
    
    df = df[, paste0(names(df)[2:length(df)], "_MS") := lapply(.SD, function(x) 100*x/sum(x)), 
            .SDcols = 2:length(df)]          
    
    df = df[, .SD, .SDcols = -c(2:7)]
    
    df$DeltaCM = df[, Reduce(`-`, .SD), .SDcols = c(3, 2)]
    df$DeltaYTD = df[, Reduce(`-`, .SD), .SDcols = c(5, 4)]
    df$DeltaMAT = df[, Reduce(`-`, .SD), .SDcols = c(7, 6)]
    
    setcolorder(df, c(1:3, 8, 4:5, 9, 6:7, 10))
    df = df[, .SD, .SDcols = -c(2, 5, 8)]
    
    result = head(df[order(-df[,4])], linesToShow)
    return(result)
}

dataChart = function(measure, level, linesToShow, filterSegments) {
    
    # Create subset which consists only segments we'll work with
    #df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
    #          by = .(Brand, PS2, PS3, PS, Company, Ynb, Mnb, Form, Additives)]
    
    df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
              by = .(Brand, PS0, PS2, PS3, PS, Company, Ynb, Mnb, PriceSegment)]
    
    if (level == "Company")  {df = data.table::dcast(df, Company~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Brand") {df = data.table::dcast(df, Brand~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Segment") {df = data.table::dcast(df, PriceSegment~Ynb+Mnb, fun = sum, value.var = measure)}
    
    nc = length(df)
    
    df$YTDLY = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No - 11):(nc - 12)]
    df$YTD = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No + 1):nc]
    df$MATLY = df[, Reduce(`+`, .SD), .SDcols = (nc - 23):(nc - 12)]
    df$MAT = df[, Reduce(`+`, .SD), .SDcols = (nc - 11):nc]
    
    df = df[, .SD, .SDcols=c(1, (nc-12):nc, (nc+1):(nc+4))]
    
    df = df[, paste0(names(df)[2:length(df)], "_MS") := lapply(.SD, function(x) 100*x/sum(x)), 
            .SDcols = 2:length(df)]          
    
    df = df[, .SD, .SDcols = -c(4:18)]
    df[,2] = NA
    df[,3] = NA
    names(df)[2:3] = c("Blank1", "Blank2")
    
    setcolorder(df, c(1, 19:20, 2, 17:18, 3, 4:16))
    
    result = head(df[order(-df[,6])], linesToShow)
    return(result)
}

dataSegmentTable = function(measure, level, linesToShow, filterSegments) {
    
    # Create subset which consists only segments we'll work with
    #df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
    #          by = .(Brand, PS2, PS3, PS, Company, Ynb, Mnb, Form, Additives)]
    
    df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
              by = .(Brand, PS0, PS2, PS3, PS, Company, Ynb, Mnb, PriceSegment)]
    
    if (level == "Company")  {df = data.table::dcast(df, Company~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Brand") {df = data.table::dcast(df, Brand~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Segment") {df = data.table::dcast(df, PriceSegment~Ynb+Mnb, fun = sum, value.var = measure)}
    
    nc = length(df)
    
    df$L3M = df[, Reduce(`+`, .SD), .SDcols = (nc - 2):nc]
    df$YTD = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No + 1):nc]
    
   
    
    df = df[, paste0(names(df)[2:length(df)], "_MS") := lapply(.SD, function(x) 100*x/sum(x)), 
            .SDcols = 2:length(df)]          
    
    df = df[, .SD, .SDcols = -c(2:(2*nc))]
 
    
    #setcolorder(df, c(1, 17:18, 19, 15:16, 20, 2:14))
    
    #result = head(df[order(-df[,6])], linesToShow)
    result = head(df[order(df[,1])], linesToShow)
    return(result)
}

dataSegmentChart = function(measure, level, linesToShow, filterSegments) {
    
    # Create subset which consists only segments we'll work with
    #df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
    #          by = .(Brand, PS2, PS3, PS, Company, Ynb, Mnb, Form, Additives)]
    
    df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
              by = .(Brand, PS0, PS2, PS3, PS, Company, Ynb, Mnb, PriceSegment)]
    
    if (level == "Company")  {df = data.table::dcast(df, Company~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Brand") {df = data.table::dcast(df, Brand~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Segment") {df = data.table::dcast(df, PriceSegment~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "ISSegment") {df = data.table::dcast(df, PS~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "IMFSegment") {df = data.table::dcast(df, PS3+PS2~Ynb+Mnb, fun = sum, value.var = measure)
                                     df = df[,PS3 := paste(PS3, PS2)][,PS2 := NULL]}
    
    nc = length(df)
    
    df$YTDLY = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No - 11):(nc - 12)]
    df$YTD = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No + 1):nc]
    df$MATLY = df[, Reduce(`+`, .SD), .SDcols = (nc - 23):(nc - 12)]
    df$MAT = df[, Reduce(`+`, .SD), .SDcols = (nc - 11):nc]
    
    df = df[, .SD, .SDcols=c(1, (nc-12):nc, (nc+1):(nc+4))]
    
    #df = df[, paste0(names(df)[2:length(df)], "_MS") := lapply(.SD, function(x) 100*x/sum(x)), 
    #        .SDcols = 2:length(df)]          
    
    #df = df[, .SD, .SDcols = -c(4:18)]
    df$Blank1 = NA
    df$Blank2 = NA
    
    setcolorder(df, c(1, 17:18, 19, 15:16, 20, 2:14))
    
    #result = head(df[order(-df[,6])], linesToShow)
    result = head(df[order(df[,1])], linesToShow)
    return(result)
}


## Update Google sheet

#tableColnames1 = c("Company", "SEP 17", "vs PP, pp", "YTD 17", "vs PY, pp", "MAT 17", "vs PY, pp")
#tableColnames2 = c("Brand", "SEP 17", "vs PP, pp", "YTD 17", "vs PY, pp", "MAT 17", "vs PY, pp")
#tableColnames3 = c("Company", "MAT 16", "MAT 17", ".", "YTD 16", "YTD 17", ".",
#                   "SEP 16", " OCT 16", "NOV 16", "DEC 16", "JAN 17", "FEB 17", "MAR 17",
#                   "APR 17", "MAY 17", "JUN 17", "JUL 17", "AUG 17", "SEP 17")
#tableColnames4 = c("Brand", "MAT 16", "MAT 17", ".", "YTD 16", "YTD 17", ".",
#                   "SEP 16", " OCT 16", "NOV 16", "DEC 16", "JAN 17", "FEB 17", "MAR 17",
#                   "APR 17", "MAY 17", "JUN 17", "JUL 17", "AUG 17", "SEP 17")
#tableColnames5 = c("Segment", "SEP 17", "L3M", "YTD 17")

tableColnames1 = c("Company", "FEB 18", "vs PP, pp", "YTD 18", "dif. vs YTD17", "MAT 18", "dif. vs MAT17")
tableColnames2 = c("Brand", "FEB 18", "vs PP, pp", "YTD 18", "dif. vs YTD17", "MAT 18", "dif. vs MAT17")
tableColnames3 = c("Company", "MAT 17", "MAT 18", ".", "YTD 17", "YTD 18", ".",
                   "FEB 17", "MAR 17",
                   "APR 17", "MAY 17", "JUN 17", "JUL 17", "AUG 17", "SEP 17", "OCT 17", 
                   "NOV 17", "DEC 17", "JAN 18", "FEB 18")
tableColnames4 = c("Brand", "MAT 17", "MAT 18", ".", "YTD 17", "YTD 18", ".",
                   "FEB 17", "MAR 17",
                   "APR 17", "MAY 17", "JUN 17", "JUL 17", "AUG 17", "SEP 17", "OCT 17", 
                   "NOV 17", "DEC 17", "JAN 18", "FEB 18")
tableColnames5 = c("Segment", "FEB 18", "L3M", "YTD 18")

gs_title("Baby Food data")
gs_object <- gs_key("1efRfjHRRgtbLwwTznmveBoKiKwx_xMsBxMy4zlwhqDs")

### COMPANIES

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A23")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 11, "VALUEC > 0"), 
              anchor="A24", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A3")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 11, "VALUEC > 0"), 
              anchor="A4", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food + Puree
gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A363")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD" | 
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS"'), 
              anchor="A364", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A383")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD" | 
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS"'), 
              anchor="A384", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food
gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A43")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD"'), 
              anchor="A44", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A63")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD"'),
              anchor="A64", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A83")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 10, 
                                'PS0 == "IMF"'), 
              anchor="A84", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A103")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 10, 
                                'PS0 == "IMF"'), 
              anchor="A104", 
              col_names=FALSE, 
              trim=FALSE)

#Dry Food
gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A123")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A143")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A144", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A163")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 10, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A164", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A183")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 10, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A184", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A203")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 10, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A204", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A223")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 10, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A224", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A243")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 10, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A244", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A263")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 10, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A264", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A283")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 10, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A284", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A303")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 10, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A304", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A323")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 10, 
                                'PS3 == "CEREAL BISCUITS"'), 
              anchor="A324", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A343")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 10, 
                                'PS3 == "CEREAL BISCUITS"'), 
              anchor="A344", 
              col_names=FALSE, 
              trim=FALSE)

## BRANDS

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A4")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, "VALUEC > 0"), 
              anchor="A5", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A23")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, "VALUEC > 0"), 
              anchor="A24", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food + Puree
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2563")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD" | 
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS"'),
              anchor="A2564", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2582")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD" | 
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS"'),
              anchor="A2583", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A44")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD"'),
              anchor="A45", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A63")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD"'),
              anchor="A64", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A83")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS0 == "IMF"'), 
              anchor="A84", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A102")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS0 == "IMF"'), 
              anchor="A103", 
              col_names=FALSE, 
              trim=FALSE)

# IMF Premium
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A123")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS0 == "IMF" & PriceSegment == "PREMIUM"'), 
              anchor="A124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A142")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS0 == "IMF" & PriceSegment == "PREMIUM"'), 
              anchor="A143", 
              col_names=FALSE, 
              trim=FALSE)

# IMF Mainstream
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A163")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS0 == "IMF" & PriceSegment == "MAINSTREAM"'), 
              anchor="A164", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A182")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS0 == "IMF" & PriceSegment == "MAINSTREAM"'), 
              anchor="A183", 
              col_names=FALSE, 
              trim=FALSE)


# IF Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A203")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A204", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A222")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A223", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base Premium
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A243")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A244", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A262")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A263", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base Mainstream
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A283")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & PriceSegment == "MAINSTREAM"'), 
              anchor="A284", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A302")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & PriceSegment == "MAINSTREAM"'), 
              anchor="A303", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A323")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A324", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A342")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A343", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base Premium
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A363")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A364", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A382")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A383", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base Mainstream
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A403")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & PriceSegment == "MAINSTREAM"'), 
              anchor="A404", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A422")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & PriceSegment == "MAINSTREAM"'), 
              anchor="A423", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A443")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A444", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A462")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A463", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base Premium
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A483")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A484", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A502")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A503", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base Mainstream
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A523")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & PriceSegment == "MAINSTREAM"'), 
              anchor="A524", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A542")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & PriceSegment == "MAINSTREAM"'), 
              anchor="A543", 
              col_names=FALSE, 
              trim=FALSE)

# Specials
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A563")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "SPECIALS"'), 
              anchor="A564", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A582")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "SPECIALS"'), 
              anchor="A583", 
              col_names=FALSE, 
              trim=FALSE)

# AT
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A603")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS == "ALLERGY TREATMENT"'), 
              anchor="A604", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A622")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS == "ALLERGY TREATMENT"'), 
              anchor="A623", 
              col_names=FALSE, 
              trim=FALSE)

# AR
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A643")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS == "ANTI REFLUX"'), 
              anchor="A644", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A662")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS == "ANTI REFLUX"'), 
              anchor="A663", 
              col_names=FALSE, 
              trim=FALSE)

# DRNL
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A683")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS == "DR-NL"'), 
              anchor="A684", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A702")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS == "DR-NL"'), 
              anchor="A703", 
              col_names=FALSE, 
              trim=FALSE)

# DC
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A723")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS == "DIGESTIVE COMFORT"'), 
              anchor="A724", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A742")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS == "DIGESTIVE COMFORT"'), 
              anchor="A743", 
              col_names=FALSE, 
              trim=FALSE)

# HA
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A763")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS == "HYPOALLERGENIC"'), 
              anchor="A764", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A782")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS == "HYPOALLERGENIC"'), 
              anchor="A783", 
              col_names=FALSE, 
              trim=FALSE)

# Preterm
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A803")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS == "PRETERM"'), 
              anchor="A804", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A822")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS == "PRETERM"'), 
              anchor="A823", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A843")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A844", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A862")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A863", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food Premium
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A883")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD" & PriceSegment == "PREMIUM"'), 
              anchor="A884", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A902")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD" & PriceSegment == "PREMIUM"'), 
              anchor="A903", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food Mainstream
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A923")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD" & PriceSegment == "MAINSTREAM"'), 
              anchor="A924", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A942")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD" & PriceSegment == "MAINSTREAM"'), 
              anchor="A943", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A963")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A964", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A982")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A983", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals Premium
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1003")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & PriceSegment == "PREMIUM"'), 
              anchor="A1004", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1022")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & PriceSegment == "PREMIUM"'), 
              anchor="A1023", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals Mainstream (doesn't work!!!)
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1043")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & PriceSegment == "MAINSTREAM"'), 
              anchor="A1044", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1062")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & PriceSegment == "MAINSTREAM"'), 
              anchor="A1063", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1083")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS"'), 
              anchor="A1084", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1102")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS"'), 
              anchor="A1103", 
              col_names=FALSE, 
              trim=FALSE)

### COMPANIES CHARTS


#IMF + Dry Food + Puree
gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A323")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Volume", "Company", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD" | 
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS"'),
              anchor="A324", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A342")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Value", "Company", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD" | 
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS"'),
              anchor="A343", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food
gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A4")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Volume", "Company", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD"'), 
              anchor="A5", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A23")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Value", "Company", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD"'), 
              anchor="A24", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A43")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Volume", "Company", 10, 
                                'PS0 == "IMF"'), 
              anchor="A44", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A62")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Value", "Company", 10, 
                                'PS0 == "IMF"'), 
              anchor="A63", 
              col_names=FALSE, 
              trim=FALSE)

#IF Base
gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A83")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Volume", "Company", 10, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A84", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A102")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Value", "Company", 10, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A103", 
              col_names=FALSE, 
              trim=FALSE)

#FO Base
gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A123")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Volume", "Company", 10, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A142")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Value", "Company", 10, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A143", 
              col_names=FALSE, 
              trim=FALSE)

#GUM Base
gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A163")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Volume", "Company", 10, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A164", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A182")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Value", "Company", 10, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A183", 
              col_names=FALSE, 
              trim=FALSE)

#Dry Food
gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A203")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Volume", "Company", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A204", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A222")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Value", "Company", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A223", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A243")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Volume", "Company", 10, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A244", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A262")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Value", "Company", 10, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A263", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A283")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Volume", "Company", 10, 
                                'PS3 == "CEREAL BISCUITS"'), 
              anchor="A284", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A302")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Value", "Company", 10, 
                                'PS3 == "CEREAL BISCUITS"'), 
              anchor="A303", 
              col_names=FALSE, 
              trim=FALSE)

## BRANDS CHARTS

#IMF + Dru Food + Puree
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2523")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD" | 
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS"'),
              anchor="A2524", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2542")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD" | 
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS"'),
              anchor="A2543", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A4")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD"'), 
              anchor="A5", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A23")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 11, 
                                'PS0 == "IMF" | PS2 == "DRY FOOD"'), 
              anchor="A24", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A43")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS0 == "IMF"'), 
              anchor="A44", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A62")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS0 == "IMF"'), 
              anchor="A63", 
              col_names=FALSE, 
              trim=FALSE)

# IMF Premium
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A83")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS0 == "IMF" & PriceSegment == "PREMIUM"'), 
              anchor="A84", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A102")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS0 == "IMF" & PriceSegment == "PREMIUM"'), 
              anchor="A103", 
              col_names=FALSE, 
              trim=FALSE)

# IMF Mainstream
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A123")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS0 == "IMF" & PriceSegment == "MAINSTREAM"'), 
              anchor="A124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A142")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS0 == "IMF" & PriceSegment == "MAINSTREAM"'), 
              anchor="A143", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A163")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A164", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A182")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A183", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base Premium
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A203")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A204", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A222")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A223", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base Mainstream
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A243")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & PriceSegment == "MAINSTREAM"'), 
              anchor="A244", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A262")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & PriceSegment == "MAINSTREAM"'), 
              anchor="A263", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A283")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A284", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A302")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A303", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base Premium
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A323")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A324", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A342")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A343", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base Mainstream
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A363")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & PriceSegment == "MAINSTREAM"'), 
              anchor="A364", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A382")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & PriceSegment == "MAINSTREAM"'), 
              anchor="A383", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A403")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A404", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A422")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A423", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base Premium
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A443")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A444", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A462")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A463", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base Mainstream
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A483")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & PriceSegment == "MAINSTREAM"'), 
              anchor="A484", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A502")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & PriceSegment == "MAINSTREAM"'), 
              anchor="A503", 
              col_names=FALSE, 
              trim=FALSE)

# Specials
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A523")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "SPECIALS"'), 
              anchor="A524", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A542")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "SPECIALS"'), 
              anchor="A543", 
              col_names=FALSE, 
              trim=FALSE)

# AT
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A563")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS == "ALLERGY TREATMENT"'), 
              anchor="A564", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A582")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS == "ALLERGY TREATMENT"'), 
              anchor="A583", 
              col_names=FALSE, 
              trim=FALSE)

# AR
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A603")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS == "ANTI REFLUX"'), 
              anchor="A604", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A622")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS == "ANTI REFLUX"'), 
              anchor="A623", 
              col_names=FALSE, 
              trim=FALSE)

# DRNL
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A643")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS == "DR-NL"'), 
              anchor="A644", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A662")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS == "DR-NL"'), 
              anchor="A663", 
              col_names=FALSE, 
              trim=FALSE)

# DC
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A683")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS == "DIGESTIVE COMFORT"'), 
              anchor="A684", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A702")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS == "DIGESTIVE COMFORT"'), 
              anchor="A703", 
              col_names=FALSE, 
              trim=FALSE)

# HA
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A723")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS == "HYPOALLERGENIC"'), 
              anchor="A724", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A742")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS == "HYPOALLERGENIC"'), 
              anchor="A743", 
              col_names=FALSE, 
              trim=FALSE)

# Preterm
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A763")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS == "PRETERM"'), 
              anchor="A764", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A782")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS == "PRETERM"'), 
              anchor="A783", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A803")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A804", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A822")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A823", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food Premium
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A843")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD" & PriceSegment == "PREMIUM"'), 
              anchor="A844", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A862")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD" & PriceSegment == "PREMIUM"'), 
              anchor="A863", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food Mainstream
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A883")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD" & PriceSegment == "MAINSTREAM"'), 
              anchor="A884", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A902")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD" & PriceSegment == "MAINSTREAM"'), 
              anchor="A903", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A923")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A924", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A942")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A943", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals Premium
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A963")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & PriceSegment == "PREMIUM"'), 
              anchor="A964", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A982")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & PriceSegment == "PREMIUM"'), 
              anchor="A983", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals Mainstream
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1003")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & PriceSegment == "MAINSTREAM"'), 
              anchor="A1004", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1022")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & PriceSegment == "MAINSTREAM"'), 
              anchor="A1023", 
              col_names=FALSE, 
              trim=FALSE)


# Cereal Biscuits
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1043")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS"'), 
              anchor="A1044", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1062")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS"'), 
              anchor="A1063", 
              col_names=FALSE, 
              trim=FALSE)

## SEGMENTS

# IMF
gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A4")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Volume", "Segment", 3, 
                                'PS0 == "IMF"'), 
              anchor="A5", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A13")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Value", "Segment", 3, 
                                'PS0 == "IMF"'), 
              anchor="A14", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A23")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Volume", "Segment", 3, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A24", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A32")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Value", "Segment", 3, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A33", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A43")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Volume", "Segment", 3, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A44", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A52")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Value", "Segment", 3, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A53", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A63")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Volume", "Segment", 3, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A64", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A72")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Value", "Segment", 3, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A73", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A83")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Volume", "Segment", 3, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A84", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A92")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Value", "Segment", 3, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A93", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A103")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Volume", "Segment", 3, 
                                       'PS2 == "DRY FOOD"'), 
              anchor="A104", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A112")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Value", "Segment", 3, 
                                       'PS2 == "DRY FOOD"'), 
              anchor="A113", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A123")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Volume", "Segment", 3, 
                                       'PS3 == "CEREAL BISCUITS"'), 
              anchor="A124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Segments", input = tableColnames5, byrow=TRUE, anchor="A132")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataSegmentTable("Value", "Segment", 3, 
                                       'PS3 == "CEREAL BISCUITS"'), 
              anchor="A133", 
              col_names=FALSE, 
              trim=FALSE)

## SEGMENTS CHARTS

# IMF
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A4")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                'PS0 == "IMF"'), 
              anchor="A5", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A13")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                'PS0 == "IMF"'), 
              anchor="A14", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A23")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A24", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A32")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A33", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A43")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A44", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A52")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A53", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A63")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A64", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A72")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A73", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A83")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                       'PS3 == "INSTANT CEREALS"'), 
              anchor="A84", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A92")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                       'PS3 == "INSTANT CEREALS"'), 
              anchor="A93", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A103")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A104", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A112")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A113", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A123")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                       'PS3 == "CEREAL BISCUITS"'), 
              anchor="A124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A132")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                       'PS3 == "CEREAL BISCUITS"'), 
              anchor="A133", 
              col_names=FALSE, 
              trim=FALSE)

## ADDITIONAL CHARTS

# IMC-IPC
gs_edit_cells(gs_object, ws="AdditionalCharts", input = tableColnames4, byrow=TRUE, anchor="A4")
gs_edit_cells(gs_object, ws="AdditionalCharts", 
              input = dataSegmentChart("Volume", "ISSegment", 2, 
                                       'PS == "IMC" | PS == "IPC"'), 
              anchor="A5", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="AdditionalCharts", input = tableColnames4, byrow=TRUE, anchor="A13")
gs_edit_cells(gs_object, ws="AdditionalCharts", 
              input = dataSegmentChart("Value", "ISSegment", 2, 
                                       'PS == "IMC" | PS == "IPC"'), 
              anchor="A14", 
              col_names=FALSE, 
              trim=FALSE)

# IMF by segments
gs_edit_cells(gs_object, ws="AdditionalCharts", input = tableColnames4, byrow=TRUE, anchor="A43")
gs_edit_cells(gs_object, ws="AdditionalCharts", 
              input = dataSegmentChart("Volume", "IMFSegment", 9, 
                                       'PS0 == "IMF"'), 
              anchor="A44", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="AdditionalCharts", input = tableColnames4, byrow=TRUE, anchor="A59")
gs_edit_cells(gs_object, ws="AdditionalCharts", 
              input = dataSegmentChart("Value", "IMFSegment", 9, 
                                       'PS0 == "IMF"'), 
              anchor="A60", 
              col_names=FALSE, 
              trim=FALSE)

### REGIONS

## Brands

# Center

#IMF + Dry Food + Puree
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2603")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "CENTER"'), 
              anchor="A2604", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2622")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "CENTER"'), 
              anchor="A2623", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1123")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "CENTER"'), 
              anchor="A1124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1142")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "CENTER"'), 
              anchor="A1143", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1162")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS0 == "IMF" & Region == "CENTER"'), 
              anchor="A1163", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1181")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS0 == "IMF" & Region == "CENTER"'), 
              anchor="A1182", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1203")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "CENTER"'), 
              anchor="A1204", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1222")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "CENTER"'), 
              anchor="A1223", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1243")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "CENTER"'), 
              anchor="A1244", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1262")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "CENTER"'), 
              anchor="A1263", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1283")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "CENTER"'), 
              anchor="A1284", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1302")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "CENTER"'), 
              anchor="A1303", 
              col_names=FALSE, 
              trim=FALSE)

# Specials
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1323")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "CENTER"'), 
              anchor="A1324", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1342")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "CENTER"'), 
              anchor="A1343", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1363")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "CENTER"'), 
              anchor="A1364", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1382")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "CENTER"'), 
              anchor="A1383", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1403")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "CENTER"'), 
              anchor="A1404", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1422")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "CENTER"'), 
              anchor="A1423", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1443")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "CENTER"'), 
              anchor="A1444", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1462")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "CENTER"'), 
              anchor="A1463", 
              col_names=FALSE, 
              trim=FALSE)

# East

#IMF + Dry Food + Puree
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2643")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "EAST"'), 
              anchor="A2644", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2662")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "EAST"'), 
              anchor="A2663", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1483")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "EAST"'), 
              anchor="A1484", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1502")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "EAST"'), 
              anchor="A1503", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1522")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS0 == "IMF" & Region == "EAST"'), 
              anchor="A1523", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1541")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS0 == "IMF" & Region == "EAST"'), 
              anchor="A1542", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1563")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "EAST"'), 
              anchor="A1564", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1582")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "EAST"'), 
              anchor="A1583", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1603")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "EAST"'), 
              anchor="A1604", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1622")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "EAST"'), 
              anchor="A1623", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1643")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "EAST"'), 
              anchor="A1644", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1662")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "EAST"'), 
              anchor="A1663", 
              col_names=FALSE, 
              trim=FALSE)

# Specials
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1683")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "EAST"'), 
              anchor="A1684", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1702")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "EAST"'), 
              anchor="A1703", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1723")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "EAST"'), 
              anchor="A1724", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1742")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "EAST"'), 
              anchor="A1743", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1763")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "EAST"'), 
              anchor="A1764", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1782")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "EAST"'), 
              anchor="A1783", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1803")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "EAST"'), 
              anchor="A1804", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1822")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "EAST"'), 
              anchor="A1823", 
              col_names=FALSE, 
              trim=FALSE)

# South

#IMF + Dry Food + Puree
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2683")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "SOUTH"'), 
              anchor="A2684", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2702")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "SOUTH"'), 
              anchor="A2703", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1843")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "SOUTH"'), 
              anchor="A1844", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1862")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "SOUTH"'), 
              anchor="A1863", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1882")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS0 == "IMF" & Region == "SOUTH"'), 
              anchor="A1883", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1901")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS0 == "IMF" & Region == "SOUTH"'), 
              anchor="A1902", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1923")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "SOUTH"'), 
              anchor="A1924", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1942")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "SOUTH"'), 
              anchor="A1943", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1963")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "SOUTH"'), 
              anchor="A1964", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A1982")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "SOUTH"'), 
              anchor="A1983", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2003")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "SOUTH"'), 
              anchor="A2004", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2022")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "SOUTH"'), 
              anchor="A2023", 
              col_names=FALSE, 
              trim=FALSE)

# Specials
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2043")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "SOUTH"'), 
              anchor="A2044", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2062")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "SOUTH"'), 
              anchor="A2063", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2083")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "SOUTH"'), 
              anchor="A2084", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2102")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "SOUTH"'), 
              anchor="A2103", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2123")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "SOUTH"'), 
              anchor="A2124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2142")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "SOUTH"'), 
              anchor="A2143", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2163")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "SOUTH"'), 
              anchor="A2164", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2182")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "SOUTH"'), 
              anchor="A2183", 
              col_names=FALSE, 
              trim=FALSE)

# West

#IMF + Dry Food + Puree
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2723")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "WEST"'), 
              anchor="A2724", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2742")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "WEST"'), 
              anchor="A2743", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2203")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "WEST"'), 
              anchor="A2204", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2222")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "WEST"'), 
              anchor="A2223", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2242")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS0 == "IMF" & Region == "WEST"'), 
              anchor="A2243", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2261")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS0 == "IMF" & Region == "WEST"'), 
              anchor="A2262", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2283")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "WEST"'), 
              anchor="A2284", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2302")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "WEST"'), 
              anchor="A2303", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2323")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "WEST"'), 
              anchor="A2324", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2342")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "WEST"'), 
              anchor="A2343", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2363")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "WEST"'), 
              anchor="A2364", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2382")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "WEST"'), 
              anchor="A2383", 
              col_names=FALSE, 
              trim=FALSE)

# Specials
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2403")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "WEST"'), 
              anchor="A2404", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2422")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "WEST"'), 
              anchor="A2423", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2443")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "WEST"'), 
              anchor="A2444", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2462")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "WEST"'), 
              anchor="A2463", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2483")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "WEST"'), 
              anchor="A2484", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2502")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "WEST"'), 
              anchor="A2503", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2523")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "WEST"'), 
              anchor="A2524", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A2542")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "WEST"'), 
              anchor="A2543", 
              col_names=FALSE, 
              trim=FALSE)

## Brands charts

# Center

#IMF + Dry Food + Puree
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2563")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "CENTER"'), 
              anchor="A2564", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2582")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "CENTER"'), 
              anchor="A2583", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1083")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "CENTER"'), 
              anchor="A1084", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1102")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "CENTER"'), 
              anchor="A1103", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1123")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS0 == "IMF" & Region == "CENTER"'), 
              anchor="A1124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1142")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS0 == "IMF" & Region == "CENTER"'), 
              anchor="A1143", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1163")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "CENTER"'), 
              anchor="A1164", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1182")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "CENTER"'), 
              anchor="A1183", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1203")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "CENTER"'), 
              anchor="A1204", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1222")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "CENTER"'), 
              anchor="A1223", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1243")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "CENTER"'), 
              anchor="A1244", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1262")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "CENTER"'), 
              anchor="A1263", 
              col_names=FALSE, 
              trim=FALSE)

# Specials
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1283")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "CENTER"'), 
              anchor="A1284", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1302")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "CENTER"'), 
              anchor="A1303", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1323")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "CENTER"'), 
              anchor="A1324", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1342")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "CENTER"'), 
              anchor="A1343", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1363")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "CENTER"'), 
              anchor="A1364", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1382")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "CENTER"'), 
              anchor="A1383", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1403")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "CENTER"'), 
              anchor="A1404", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1422")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "CENTER"'), 
              anchor="A1423", 
              col_names=FALSE, 
              trim=FALSE)

# East

#IMF + Dry Food + Puree
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2603")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "EAST"'), 
              anchor="A2604", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2622")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "EAST"'), 
              anchor="A2623", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1443")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "EAST"'), 
              anchor="A1444", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1462")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "EAST"'), 
              anchor="A1463", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1483")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS0 == "IMF" & Region == "EAST"'), 
              anchor="A1484", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1502")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS0 == "IMF" & Region == "EAST"'), 
              anchor="A1503", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1523")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "EAST"'), 
              anchor="A1524", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1542")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "EAST"'), 
              anchor="A1543", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1563")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "EAST"'), 
              anchor="A1564", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1582")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "EAST"'), 
              anchor="A1583", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1603")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "EAST"'), 
              anchor="A1604", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1622")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "EAST"'), 
              anchor="A1623", 
              col_names=FALSE, 
              trim=FALSE)

# Specials
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1643")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "EAST"'), 
              anchor="A1644", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1662")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "EAST"'), 
              anchor="A1663", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1683")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "EAST"'), 
              anchor="A1684", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1702")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "EAST"'), 
              anchor="A1703", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1723")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "EAST"'), 
              anchor="A1724", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1742")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "EAST"'), 
              anchor="A1743", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1763")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "EAST"'), 
              anchor="A1764", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1782")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "EAST"'), 
              anchor="A1783", 
              col_names=FALSE, 
              trim=FALSE)

# South

#IMF + Dry Food + Puree
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2643")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "SOUTH"'), 
              anchor="A2644", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2662")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "SOUTH"'), 
              anchor="A2663", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Dry Food
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1803")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "SOUTH"'), 
              anchor="A1804", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1822")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "SOUTH"'), 
              anchor="A1823", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1843")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS0 == "IMF" & Region == "SOUTH"'), 
              anchor="A1844", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1862")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS0 == "IMF" & Region == "SOUTH"'), 
              anchor="A1863", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1883")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "SOUTH"'), 
              anchor="A1884", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1902")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "SOUTH"'), 
              anchor="A1903", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1923")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "SOUTH"'), 
              anchor="A1924", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1942")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "SOUTH"'), 
              anchor="A1943", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1963")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "SOUTH"'), 
              anchor="A1964", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A1982")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "SOUTH"'), 
              anchor="A1983", 
              col_names=FALSE, 
              trim=FALSE)

# Specials
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2003")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "SOUTH"'), 
              anchor="A2004", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2022")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "SOUTH"'), 
              anchor="A2023", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2043")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "SOUTH"'), 
              anchor="A2044", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2062")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "SOUTH"'), 
              anchor="A2063", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2083")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "SOUTH"'), 
              anchor="A2084", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2102")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "SOUTH"'), 
              anchor="A2103", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2123")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "SOUTH"'), 
              anchor="A2124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2142")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "SOUTH"'), 
              anchor="A2143", 
              col_names=FALSE, 
              trim=FALSE)

# West

#IMF + Dry Food + Puree
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2683")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "WEST"'), 
              anchor="A2684", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2702")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD" |
                                PS3 == "SAVOURY MEAL" |
                                PS3 == "FRUITS") & Region == "WEST"'), 
              anchor="A2703", 
              col_names=FALSE, 
              trim=FALSE)


#IMF + Dry Food
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2163")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "WEST"'), 
              anchor="A2164", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2182")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 11, 
                                '(PS0 == "IMF" | PS2 == "DRY FOOD") & Region == "WEST"'), 
              anchor="A2183", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2203")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS0 == "IMF" & Region == "WEST"'), 
              anchor="A2204", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2222")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS0 == "IMF" & Region == "WEST"'), 
              anchor="A2223", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2243")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "WEST"'), 
              anchor="A2244", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2262")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & Region == "WEST"'), 
              anchor="A2263", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2283")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "WEST"'), 
              anchor="A2284", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2302")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & Region == "WEST"'), 
              anchor="A2303", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2323")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "WEST"'), 
              anchor="A2324", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2342")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & Region == "WEST"'), 
              anchor="A2343", 
              col_names=FALSE, 
              trim=FALSE)

# Specials
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2363")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "WEST"'), 
              anchor="A2364", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2382")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "SPECIALS" & Region == "WEST"'), 
              anchor="A2383", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2403")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "WEST"'), 
              anchor="A2404", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2422")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD" & Region == "WEST"'), 
              anchor="A2423", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2443")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "WEST"'), 
              anchor="A2444", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2462")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS" & Region == "WEST"'), 
              anchor="A2463", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2483")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "WEST"'), 
              anchor="A2484", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A2502")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS" & Region == "WEST"'), 
              anchor="A2503", 
              col_names=FALSE, 
              trim=FALSE)

# Price Segments charts

## Center

# IMF
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A146")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                       'PS0 == "IMF" & Region == "CENTER"'), 
              anchor="A147", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A155")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                       'PS0 == "IMF" & Region == "CENTER"'), 
              anchor="A156", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A166")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                       'PS3 == "INSTANT CEREALS" & Region == "CENTER"'), 
              anchor="A167", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A175")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                       'PS3 == "INSTANT CEREALS" & Region == "CENTER"'), 
              anchor="A176", 
              col_names=FALSE, 
              trim=FALSE)

## East

# IMF
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A186")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                       'PS0 == "IMF" & Region == "EAST"'), 
              anchor="A187", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A195")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                       'PS0 == "IMF" & Region == "EAST"'), 
              anchor="A196", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A206")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                       'PS3 == "INSTANT CEREALS" & Region == "EAST"'), 
              anchor="A207", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A215")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                       'PS3 == "INSTANT CEREALS" & Region == "EAST"'), 
              anchor="A216", 
              col_names=FALSE, 
              trim=FALSE)

## South

# IMF
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A226")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                       'PS0 == "IMF" & Region == "SOUTH"'), 
              anchor="A227", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A235")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                       'PS0 == "IMF" & Region == "SOUTH"'), 
              anchor="A236", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A246")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                       'PS3 == "INSTANT CEREALS" & Region == "SOUTH"'), 
              anchor="A247", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A255")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                       'PS3 == "INSTANT CEREALS" & Region == "SOUTH"'), 
              anchor="A256", 
              col_names=FALSE, 
              trim=FALSE)

## West

# IMF
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A266")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                       'PS0 == "IMF" & Region == "WEST"'), 
              anchor="A267", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A275")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                       'PS0 == "IMF" & Region == "WEST"'), 
              anchor="A276", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A286")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Volume", "Segment", 3, 
                                       'PS3 == "INSTANT CEREALS" & Region == "WEST"'), 
              anchor="A287", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A295")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataSegmentChart("Value", "Segment", 3, 
                                       'PS3 == "INSTANT CEREALS" & Region == "WEST"'), 
              anchor="A296", 
              col_names=FALSE, 
              trim=FALSE)