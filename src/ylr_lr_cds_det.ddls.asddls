@AbapCatalog.sqlViewName: 'YLR_NEW_DET'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'LR DET NEW'

define view YLR_LR_CDS_DET as select from I_BillingDocumentBasic as a
left outer join I_BillingDocumentPartner as b
    on ( b.BillingDocument = a.BillingDocument and  b.PartnerFunction = 'WE'   )
left outer join I_Customer as C on ( C.Customer = b.Customer  )    
left outer join I_BillingDocumentPartner as D on ( D.BillingDocument = a.BillingDocument
  and D.PartnerFunction = 'ZR' )
left outer join I_Supplier as E on ( E.Supplier = D.Supplier  ) 
left outer join I_BillingDocumentPartner as F on ( F.BillingDocument = a.BillingDocument
  and F.PartnerFunction = 'ZP' )  
left outer join I_Supplier as G on ( G.Supplier = F.Supplier  )
left outer join I_BillingDocumentPartner as H
    on ( H.BillingDocument = a.BillingDocument and  b.PartnerFunction = 'AG'   )
left outer join I_Customer as I on ( I.Customer = H.Customer  )    
 
  {
   key a.BillingDocument as InvoiceNo,
       a.BillingDocumentDate,
   C.Customer  as Shiptoparty,
   C.CustomerName as ShiptopartyName,
   C.CityName as shiptopartycity,
   //F.YY1_LRNo_BDH,
   a.BillingDocument as LRNO,
//   a.YY1_VehicleNo_BDH as VehicalNo,
   E.SupplierName as TransporterName,
   G.SupplierName as SubtransporterName,
   I.Customer  as Soldtoparty,
   I.CustomerName as SoldtopartyName,
   I.CityName as Soldtopartycity    
}
