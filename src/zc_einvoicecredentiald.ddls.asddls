@EndUserText.label: 'E Invoice credential details - Maintain'
@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
define view entity ZC_EInvoiceCredentialD
  as projection on ZI_EInvoiceCredentialD
{
  key CompanyCode,
  key TaxNumber3,
  key Region,
  Name1,
  EwaybillPass,
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  @Consumption.hidden: true
  LastChangedAt,
  @Consumption.hidden: true
  LocalLastChangedAt,
  @Consumption.hidden: true
  SingletonID,
  _EInvoiceCredentiAll : redirected to parent ZC_EInvoiceCredentialD_S
  
}
