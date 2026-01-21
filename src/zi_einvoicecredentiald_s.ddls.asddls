@EndUserText.label: 'E Invoice credential details Singleton'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZI_EInvoiceCredentialD_S
  as select from I_Language
    left outer join ZEINV_CREDENTIAL on 0 = 0
  composition [0..*] of ZI_EInvoiceCredentialD as _EInvoiceCredentialD
{
  key 1 as SingletonID,
  _EInvoiceCredentialD,
  max( ZEINV_CREDENTIAL.LOCAL_LAST_CHANGED_AT ) as LastChangedAtMax,
  cast( '' as SXCO_TRANSPORT) as TransportRequestID,
  cast( 'X' as ABAP_BOOLEAN preserving type) as HideTransport
  
}
where I_Language.Language = $session.system_language
