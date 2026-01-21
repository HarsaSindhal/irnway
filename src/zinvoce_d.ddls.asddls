@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'invoce_d'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zinvoce_d as select from  I_BillingDocumentItem as a 
left outer join I_SalesDocumentItem as b on ( b.SalesDocument = a.SalesDocument and b.SalesDocumentItem = a.SalesDocumentItem )
 
{
key a.BillingDocument,
key a.BillingDocumentItem,
a.Plant,
a.Material,
b.SalesDocumentType,
b.SalesDocument,
b.SalesDocumentItem

 //b.YY1_HSNCode_SDI 

}

where (b.SalesDocumentType = 'G2'  or b.SalesDocumentType = 'L2'  )
