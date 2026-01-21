@EndUserText.label: 'PORT OF LOADING Singleton'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZI_PortOfLoading_S
  as select from I_Language
    left outer join ZPORT_LOADING on 0 = 0
  composition [0..*] of ZI_PortOfLoading as _PortOfLoading
{
  key 1 as SingletonID,
  _PortOfLoading,
  max( ZPORT_LOADING.LOCAL_LAST_CHANGED_AT ) as LastChangedAtMax,
  cast( '' as SXCO_TRANSPORT) as TransportRequestID,
  cast( 'X' as ABAP_BOOLEAN preserving type) as HideTransport
  
}
where I_Language.Language = $session.system_language
