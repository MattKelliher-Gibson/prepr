FieldAnalysis <- function(datatable, field, invalid_type) {
#####################################################
# Returns the Number of Missing and Invlaid Records	#
#													#
# Creator: Matthew Kelliher-Gibson					#
# Created: 04/29/2015								#
# Stage: BETA										#
#													#
# Args:												#
#	datatable: Datatable to be Evaluated			#
#	field: Quoted Name of Field to be Evaluated		#
#	invalid_type: One of the Following				#
#		Alpha: Checks for Only Alpha Characters		#
#		Numeric: Checks for Only Numeric Characters	#
#		Both: Checks for Only Numeric or Only Alpha	#
#		Zip: Checks for Invalid Zip Codes			#
#		State: Checks for Invalid State Formats		#
#		Phone: Checks for Invalid Phone Formats		#
#		Email: Checks for Invalid Email Formats		#
#													#
# Returns:											#
#	List of 2										#
#		Missing: Number of Missing Records			#
#		Invalid: Number of Invalid Format Records	#
#													#
# Dependencies:										#
#	data.table										#
#	stringr											#
#													#
# Version History:									#
#	0.0.0 - 04/29/2015 - Initial Creation			#
#	0.1.0 - 07/15/2015 - Format and Update			#
#####################################################

#A. Check for Required Packages:

	#1. StringR
	
		if (!require(stringr)) stop("stringr Package Required")
		
#B. Check Data Table

	if (!(is.data.table(datatable))) stop("datatable must be a Data Table")
	
#C. Execute Function

	#1. Create Empty List
	
		results <- list()
    
	#2. Calculate Missing Records
	
		results['Missing'] <- datatable[is.na(eval(field)) | str_trim(eval(field)) == '', .N]
    
	#3. Calculate Invalid Records
	
		if (invalid_type == "Alpha")
		  {
			results['Invalid'] <- datatable[grepl('^[[:alpha:]]$', eval(field), perl = TRUE) | grepl('^[^[:alnum:]]+$', eval(field), perl = TRUE), .N]
		  }
		else if (invalid_type == "Numeric")
		  {
			results['Invalid'] <- datatable[grepl('^[[:digit:]]$', eval(field), perl = TRUE) | grepl('^[^[:alnum:]]+$', eval(field), perl = TRUE), .N]
		  }
		else if (invalid_type == "Both")
		  {
			results['Invalid'] <- datatable[grepl('^[[:digit:]]+$', eval(field), perl = TRUE) | grepl('^[[:alpha:]]+$', eval(field), perl = TRUE) | grepl('^[^[:alnum:]]+$', eval(field), perl = TRUE), .N]
		  }
		else if (invalid_type == "Zip")
		  {
			results['Invalid'] <- datatable[!(grepl('^[0-9]{5}$', eval(field), perl = TRUE)) & grepl('^[^[:alnum:]]+$', eval(field)) & !(grepl('^[0-9]{5}-[0-9]{4}$', eval(field), perl = TRUE)) & !(is.na(eval(field)) | str_trim(eval(field)) == ''), .N]
		  }
		else if (invalid_type == "State")
		  {
			results['Invalid'] <- datatable[nchar(eval(field)) != 2 | grepl('^[^[:alnum:]]+$', eval(field), perl = TRUE) | grepl('[[:digit:]]+', eval(field), perl = TRUE), .N]
		  }
		else if (invalid_type == "Phone")
		  {
		  results['Invalid'] <- datatable[!(grepl('^[0-9]{7}$', eval(field), perl = TRUE)) & !(grepl('^[0-9]{10}$', eval(field), perl = TRUE)) & !(grepl('[0-9]{3}-[0-9]{3}-[0-9]{4}', eval(field), perl = TRUE)) & !(grepl('[0-9]{3}-[0-9]{4}', eval(field), perl = TRUE)) & !(is.na(eval(field))) & !(str_trim(eval(field)) == ''), .N]
		  }
		else if (invalid_type == "Email")
		  {
			results['Invalid'] <- datatable[!(grepl('^[[:graph:]+@[[:graph:]]+[.][A-z]{2,3}$', eval(field), perl = TRUE)), .N]
		  }
	#4. Return Results
	
		return(results)
  }