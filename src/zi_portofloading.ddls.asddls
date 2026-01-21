@EndUserText.label: 'PORT OF LOADING'
@AccessControl.authorizationCheck: #CHECK
define view entity ZI_PortOfLoading
  as select from ZPORT_LOADING
  association to parent ZI_PortOfLoading_S as _PortOfLoadingAll on $projection.SingletonID = _PortOfLoadingAll.SingletonID
{
  key PORTCODE as Portcode,
  DISCRIPTION as Discription,
  CREATED_BY as CreatedBy,
  CREATED_AT as CreatedAt,
  LAST_CHANGED_BY as LastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  LAST_CHANGED_AT as LastChangedAt,
  @Semantics.systemDateTime.lastChangedAt: true
  LOCAL_LAST_CHANGED_AT as LocalLastChangedAt,
  1 as SingletonID,
  _PortOfLoadingAll
  
}
