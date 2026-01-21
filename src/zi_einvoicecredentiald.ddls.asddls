@EndUserText.label: 'E Invoice credential details'
@AccessControl.authorizationCheck: #CHECK
define view entity ZI_EInvoiceCredentialD
  as select from zeinv_credential
  association to parent ZI_EInvoiceCredentialD_S as _EInvoiceCredentiAll on $projection.SingletonID = _EInvoiceCredentiAll.SingletonID
{
  key company_code as CompanyCode, 
  key tax_number3 as TaxNumber3,
  key region as Region,
  name1 as Name1,
  ewaybill_pass as EwaybillPass, 
  created_by as CreatedBy,
  created_at as CreatedAt,
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  last_changed_at as LastChangedAt,
  @Semantics.systemDateTime.lastChangedAt: true
  local_last_changed_at as LocalLastChangedAt,
  1 as SingletonID,
  _EInvoiceCredentiAll
  
}  
