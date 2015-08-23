JoinNA_list <- function(x, join_type, by = NULL, fill = 0, ...){
#########################################################
# Joins a list of Data Frames and fills all NA values   #
#                                                       #
# Creator: Matthew Kelliher-Gibson                      #
# Created: 07/20/2015                                   #
# Stage: ALPHA                                          #
#                                                       #
# Args:                                                 #
#   x: list of Data Frames                              #
#   join_type: non-quoted join type (e.g. left_join)    #
#   by: (NULL) quoted variable to join ALL Data Frames  #
#   fill: Value to Fill all NA values                   #
#   ... An other agruements to pass to join function    #
#                                                       #
# Returns:                                              #
#   Joined Data Frame                                   #
#                                                       #
# Dependencies:                                         #
#   dplyr                                               #
#                                                       #
# Version History:                                      #
#   0.0.0 - 07/20/2015 - Inital Creation                #
#########################################################
  join_all(x, by = by, type = join_type, ...) %>%
    mutate_each(funs(replace(., which(is.na(.)), fill)))
}