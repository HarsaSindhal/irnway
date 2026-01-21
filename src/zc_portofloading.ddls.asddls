@EndUserText.label: 'PORT OF LOADING - Maintain'
@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
define view entity ZC_PortOfLoading
  as projection on ZI_PortOfLoading
{
  key Portcode,
  Discription,
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  @Consumption.hidden: true
  LastChangedAt,
  @Consumption.hidden: true
  LocalLastChangedAt,
  @Consumption.hidden: true
  SingletonID,
  _PortOfLoadingAll : redirected to parent ZC_PortOfLoading_S
  
}
