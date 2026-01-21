@EndUserText.label: 'E Invoice credential details Singleton -'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: [ 'SingletonID' ]
define root view entity ZC_EInvoiceCredentialD_S
  provider contract transactional_query
  as projection on ZI_EInvoiceCredentialD_S
{
  key SingletonID,
  LastChangedAtMax,
  TransportRequestID,
  HideTransport,
  _EInvoiceCredentialD : redirected to composition child ZC_EInvoiceCredentialD
  
}
