@AbapCatalog.sqlViewName: 'YLR_CDS_DET'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'LR Detail'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view YLR_CDS_VIEW as select from I_BillingDocumentItem as a
inner join I_BillingDocumentItemPrcgElmnt as b
    on a.BillingDocument = b.BillingDocument and b.ConditionType = 'ZTRA'    
    {
  key 
    a.Plant,
    a.BillingDocument as InvoiceNo, 
    a.BillingQuantityUnit,
  @Semantics.quantity.unitOfMeasure: 'BillingQuantityUnit'
    sum(a.BillingQuantity ) as InvoiceQuantity,
    b.ConditionCurrency,
  @Semantics.amount.currencyCode: 'ConditionCurrency'
    sum(b.ConditionRateValue ) as Condition_Amt,
    b.TransactionCurrency
   //  ( a.BillingQuantity * b.ConditionRateValue ) as FrieghtAmount
} group by a.BillingDocument,
           a.Plant,
           a.BillingQuantityUnit,
           b.TransactionCurrency,
           b.ConditionCurrency;

//a.BillingQuantity,b.ConditionRateValue;
          
          
  
