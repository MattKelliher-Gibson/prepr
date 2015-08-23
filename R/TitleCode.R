TitleCode <- function(var) {
#################################################
# Replace RSM Product Code with Product Name	#
#												#
# Creator: Matthew Kelliher-Gibson				#
# Created: 07/01/2015							#
# Stage: BETA									#
#												#
# Args:											#
#	var: Character Vector of Product Codes		#
#												#
# Returns:										#
#	Character Vector of Product Names			#
#												#
# Dependencies:									#
#	N/A											#
#												#
# Version History:								#
#	0.0.0 - 07/01/2015 - Initial Creation		#
#	0.0.1 - 07/15/2015 - Format					#
#################################################

  switch(EXPR = var,
         '66001' = 'BuildingConstructionCostData',
         '66002' = 'MechanicalCostData',
         '66003' = 'ElectricalCostData',
         '66004' = 'CommercialRenovationCostData',
         '66005' = 'SquareFootCosts',
         '66006' = 'AssembliesCostData',
         '66009' = 'InteriorCostData',
         '66011' = 'ConcreteMasonryCostData',
         '66016' = 'HeavyConstructionCostData',
         '66017' = 'ResidentialCostData',
         '66020' = 'FacilitiesConstructionCostData',
         '66021' = 'PlumbingCostData',
         '66028' = 'SiteWorkLandscapeCostData',
         '66030' = 'FacilitiesMaintenanceRepairCostData',
         '66100' = 'ConstructionCostEstimator',
         '66110' = 'BuildersPackage',
         '66120' = 'BuildingProfessionalsPackage',
         '66130' = 'FacilitiesManagersPackage',
         '66140' = 'DesignProfessoinalPackage',
         '66150' = 'CompleteOnlineLibrary',
         '66160' = 'ConstructionCostEstimator',
         '66200' = 'CommercialComposite',
         '66201' = 'CivilComposite',
         '66202' = 'FacilityManagerPackages',
         '66203' = 'MasterUnionComposite',
         '66205' = 'OpenShopBuildersPackage',
         '66210' = 'StudentEdition',
         '66302' = 'BCCDeBookBundle',
         '66307' = 'FacilitiesMaintenanceRepairCostData',
         '66314' = 'SiteWorkLandscapeCostDataeBookBundle',
         '66330' = 'GreenBuildingCostData',
         '663YR' = '3YearHistoryAddOn',
         '66HIS' = 'FullHistoryAddOn',
         '66OPN' = 'OpenShopWageRatesAddOn'
          )
}