@AbapCatalog.sqlViewName: 'YLR_FRIGHT_REP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'LR  Report'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view YLR__LR_FRIGHT_REP as select from  YLR_CDS_VIEW as a
left outer join YLR_LR_CDS_DET as b
    on  a.InvoiceNo = b.InvoiceNo
  {
  key a.InvoiceNo,
      a.Plant,
      a.InvoiceQuantity,
      a.BillingQuantityUnit,     
      a.Condition_Amt,
      a.ConditionCurrency,
    @Semantics.quantity.unitOfMeasure: 'BillingQuantityUnit'
    @Semantics.amount.currencyCode: 'ConditionCurrency'
      ( a.InvoiceQuantity * a.Condition_Amt ) as FrightAmount
//      ,
//      b.TransporterName,
//      b.SubtransporterName,
//      b.Shiptoparty,
//      b.ShiptopartyName,
//      b.shiptopartycity,
//      b.LRNO,
//      b.VehicalNo,
//      b.Soldtoparty,
//      b.SoldtopartyName,
//      b.Soldtopartycity, 
//      b.BillingDocumentDate  
}
