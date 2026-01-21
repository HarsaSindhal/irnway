@EndUserText.label: 'PORT OF LOADING Singleton - Maintain'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: [ 'SingletonID' ]
define root view entity ZC_PortOfLoading_S
  provider contract transactional_query
  as projection on ZI_PortOfLoading_S
{
  key SingletonID,
  LastChangedAtMax,
  TransportRequestID,
  HideTransport,
  _PortOfLoading : redirected to composition child ZC_PortOfLoading
  
}
