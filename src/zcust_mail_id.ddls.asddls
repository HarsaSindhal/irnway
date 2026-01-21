@AbapCatalog.sqlViewName: 'ZMAIL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Mail ID'
define view Zcust_Mail_ID as select from I_Customer as A 
 left outer join I_AddrCurDefaultEmailAddress as B on ( A.AddressID = B.AddressID )
{
  A.AddressID,
  A.Customer,
  B.EmailAddress,
  B.AddressID    as ADDRESS_ID,
  B.AddressPersonID
  
 
}
