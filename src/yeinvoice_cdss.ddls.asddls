@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Yenvoice Cds For E-envoicing'
define root view entity YEINVOICE_CDSS as select from
    I_BillingDocumentItem          as a

    left outer join I_BillingDocumentPartner       as bILLINGPARTNR            on  a.BillingDocument             = bILLINGPARTNR.BillingDocument
                                                                               and bILLINGPARTNR.PartnerFunction = 'RE'
                                                                              
    left outer join I_BillingDocumentBasic         as DOCHEAD                  on a.BillingDocument = DOCHEAD.BillingDocument
    left outer join I_ProductUnitsOfMeasure         as UN                     on UN.Product = a.Material and ( UN.AlternativeUnit = 'CS' or UN.AlternativeUnit = 'BAG' )
    left outer join I_BillingDocumentPartner     as SHIPPINGPARTNR            on  a.BillingDocument              = SHIPPINGPARTNR.BillingDocument
                                                                              // and a.BillingDocumentItem          =   SHIPPINGPARTNR.BillingDocumentItem
                                                                               and SHIPPINGPARTNR.PartnerFunction = 'WE'
  
   left outer join I_BillingDocumentPartner     as transporterPARTNR           on  a.BillingDocument              = transporterPARTNR.BillingDocument
                                                                               and transporterPARTNR.PartnerFunction = 'SP'
   
    left outer join I_BillingDocumentPartner       as SoldTOpaty              on  a.BillingDocument                 = SoldTOpaty.BillingDocument
                                                                               and SoldTOpaty.PartnerFunction = 'AG'

    left outer join I_BillingDocumentPartner       as Payerparty              on  a.BillingDocument                 = Payerparty.BillingDocument
                                                                               and Payerparty.PartnerFunction = 'RG'
                                                                               
    left outer join I_Customer                  as bILLINGPARTNRADDRESS     on bILLINGPARTNR.Customer = bILLINGPARTNRADDRESS.Customer

   left outer join I_Customer                   as SoldTOpatyADDRESS        on SoldTOpaty.Customer = SoldTOpatyADDRESS.Customer
 
   left outer join I_Customer                   as  PayerpartyRADDRESS      on Payerparty.Customer = PayerpartyRADDRESS.Customer
 
    left outer join I_Supplier                  as transporterPARTNRADDRESS on transporterPARTNR.Supplier = transporterPARTNRADDRESS.Supplier
    left outer join I_Supplier                  as trans on trans.Supplier = DOCHEAD.YY1_Transporter_BDH


 
    left outer join I_Customer                     as SHIPPINGPARTNRADDRESS    on SHIPPINGPARTNR.Customer = SHIPPINGPARTNRADDRESS.Customer

    left outer join ZI_BillingDocumentItemPrcgElmn as ZR00                     on  a.BillingDocument     = ZR00.BillingDocument
                                                                               and a.BillingDocumentItem = ZR00.BillingDocumentItem
                                                                                and (  ZR00.ConditionType    = 'ZR00' or ZR00.ConditionType    = 'ZR01'  )
                                                                                and ZR00.ConditionAmount is not initial 
                                                                                
    left outer join ZI_BillingDocumentItemPrcgElmn as PCIP                     on  a.BillingDocument     = PCIP.BillingDocument
                                                                                and  a.BillingDocumentItem = PCIP.BillingDocumentItem
                                                                                and (  PCIP.ConditionType    = 'PCIP' )
                                                                                and PCIP.ConditionAmount is not initial                                                                              

   left outer join ZI_BillingDocumentItemPrcgElmn as JOIG                     on  a.BillingDocument     = JOIG.BillingDocument
                                                                               and a.BillingDocumentItem = JOIG.BillingDocumentItem
                                                                               and JOIG.ConditionAmount is not initial 
                                                                               and JOIG.ConditionType    = 'JOIG'

    left outer join ZI_BillingDocumentItemPrcgElmn as JOCG                     on  a.BillingDocument     = JOCG.BillingDocument
                                                                               and a.BillingDocumentItem = JOCG.BillingDocumentItem
                                                                               and JOCG.ConditionAmount is not initial 
                                                                               and JOCG.ConditionType    = 'JOCG'
    left outer join ZI_BillingDocumentItemPrcgElmn as JOSG                     on  a.BillingDocument     = JOSG.BillingDocument
                                                                               and a.BillingDocumentItem = JOSG.BillingDocumentItem
                                                                               and JOSG.ConditionAmount is not initial 
                                                                               and JOSG.ConditionType    = 'JOSG'

   left outer join ZI_BillingDocumentItemPrcgElmn as ZTCS                      on  a.BillingDocument     = ZTCS.BillingDocument
                                                                               and a.BillingDocumentItem = ZTCS.BillingDocumentItem
                                                                               and ZTCS.ConditionAmount is not initial 
                                                                               and ZTCS.ConditionType    = 'ZTCS'
                                                                               
//  left outer join ZI_BillingDocumentItemPrcgElmn as D100                     on  a.BillingDocument     = D100.BillingDocument
//                                                                               and a.BillingDocumentItem = D100.BillingDocumentItem
//                                                                               and D100.ConditionAmount is not initial 
//                                                                               and ( D100.ConditionType    = 'D100'  )

  left outer join ZI_BillingDocumentItemPrcgElmn as ZDIS                     on  a.BillingDocument     = ZDIS.BillingDocument
                                                                               and a.BillingDocumentItem = ZDIS.BillingDocumentItem
                                                                               and ZDIS.ConditionAmount is not initial 
                                                                               and ( ZDIS.ConditionType    = 'ZDIS'  )
                                                                                                                                                      
 left outer join ZI_BillingDocumentItemPrcgElmn as ZSHE                       on  a.BillingDocument     = ZSHE.BillingDocument
                                                                               and a.BillingDocumentItem = ZSHE.BillingDocumentItem
                                                                               and ZSHE.ConditionAmount is not initial 
                                                                                and ZSHE.ConditionType    = 'ZSHE '

  left outer join ZI_BillingDocumentItemPrcgElmn as ZDQT                       on  a.BillingDocument     = ZDQT.BillingDocument
                                                                               and a.BillingDocumentItem = ZDQT.BillingDocumentItem
                                                                               and ZDQT.ConditionAmount is not initial 
                                                                                and ZDQT.ConditionType    = 'ZDQT'
                                                                                
  left outer join ZI_BillingDocumentItemPrcgElmn as ZCDI                       on  a.BillingDocument     = ZCDI.BillingDocument
                                                                               and a.BillingDocumentItem = ZCDI.BillingDocumentItem
                                                                               and ZCDI.ConditionAmount is not initial 
                                                                                and ZCDI.ConditionType    = 'ZCDI'
                                                                                
  left outer join ZI_BillingDocumentItemPrcgElmn as ZCDE                       on  a.BillingDocument     = ZCDE.BillingDocument
                                                                               and a.BillingDocumentItem = ZCDE.BillingDocumentItem
                                                                               and ZCDE.ConditionAmount is not initial 
                                                                                and ZCDE.ConditionType    = 'ZCDE'                                                                                
    left outer join ZI_BillingDocumentItemPrcgElmn as ZDIH                       on  a.BillingDocument     = ZDIH.BillingDocument
                                                                               and a.BillingDocumentItem = ZDIH.BillingDocumentItem
                                                                               and ZDIH.ConditionAmount is not initial 
                                                                                and ZDIH.ConditionType    = 'ZDIH'
                                                                                
 left outer join ZI_BillingDocumentItemPrcgElmn as ZDOF                       on  a.BillingDocument     = ZDOF.BillingDocument
                                                                               and a.BillingDocumentItem = ZDOF.BillingDocumentItem
                                                                               and ZDOF.ConditionAmount is not initial 
                                                                                and ZDOF.ConditionType    = 'ZDOF'
   left outer join ZI_BillingDocumentItemPrcgElmn as ZFO1                     on  a.BillingDocument     = ZFO1.BillingDocument
                                                                               and a.BillingDocumentItem = ZFO1.BillingDocumentItem
                                                                               and ZFO1.ConditionAmount is not initial 
                                                                               and ( ZFO1.ConditionType = 'ZFO1' or ZFO1.ConditionType = 'ZFO2'  or ZFO1.ConditionType = 'ZFO3'  )  
                                                                                
 left outer join I_ProductText                  as maktx                    on  a.Material     = maktx.Product
                                                                               and maktx.Language = 'E' 

    left outer join I_ProductPlantBasic            as HSNCODE                  on  a.Material = HSNCODE.Product
                                                                               and a.Plant    = HSNCODE.Plant
    left outer join YJ1IG_EWAYBILLDD               as EWAYBILL                 on a.BillingDocument = EWAYBILL.Docno and a.CompanyCode = EWAYBILL.Bukrs and EWAYBILL.Status = 'A'
    left outer join Y1IG_INVREFNUM_DD              as IRNDETALS                on a.BillingDocument = IRNDETALS.Docno and a.CompanyCode = IRNDETALS.Bukrs and IRNDETALS.IrnStatus = 'ACT'
    left outer join I_DeliveryDocument             as DELIVERYDATA             on a.ReferenceSDDocument = DELIVERYDATA.DeliveryDocument
    left outer join I_SalesDocument                as SalesDATA                on a.SalesDocument = SalesDATA.SalesDocument
   


{ 

key a.BillingDocument,
key a.BillingDocumentItem,
//a.Batch,
a.CompanyCode,
a.Plant,
a.Material,
UN.AlternativeUnit,
cast( UN.QuantityNumerator as abap.dec( 15, 3 )) as QuantityNumerator ,
a.Division,
a.DistributionChannel,
a.SalesDocument as SDDOCU,
a.SalesDocumentItem as SDDOCUITEM ,
a.BillingDocumentItemText as MaterialDescription,
HSNCODE.ConsumptionTaxCtrlCode                                                                 as Hsncode,
a.BillingQuantityUnit,
@Semantics.quantity.unitOfMeasure: 'BillingQuantityUnit' 
a.BillingQuantity,
a.TransactionCurrency,
a.BillingQuantityUnit as unit,
@Semantics.amount.currencyCode: 'TransactionCurrency'
a.NetAmount, 
@Semantics.amount.currencyCode: 'TransactionCurrency'
a.GrossAmount,
@Semantics.amount.currencyCode: 'TransactionCurrency'
a.TaxAmount, 

a.ReferenceSDDocument as DELIVERY_NUMBER ,
a.ReferenceSDDocumentItem as DELIVERY_NUMBER_item ,
//a.YY1_NoofBags2_BDI ,
bILLINGPARTNR.Customer as BILLTOPARTY,
bILLINGPARTNRADDRESS.CustomerName,
bILLINGPARTNRADDRESS.CustomerFullName,
bILLINGPARTNRADDRESS.TaxNumber3 as billinggstin,
bILLINGPARTNRADDRESS.StreetName, 
bILLINGPARTNRADDRESS.Region, 
bILLINGPARTNRADDRESS.CityName,
bILLINGPARTNRADDRESS.PostalCode,

SHIPPINGPARTNR.Customer as ShippingPartner,
SHIPPINGPARTNRADDRESS.CustomerName as  SHIPTONAME,
SHIPPINGPARTNRADDRESS.CustomerFullName as  SHIPTOFULLNAME,
SHIPPINGPARTNRADDRESS.StreetName as  SHIPTOADDRSS,
SHIPPINGPARTNRADDRESS.TaxNumber3 as SHIPPINGPARTNRgstin,
SHIPPINGPARTNRADDRESS.Region as  SHIPTOREGION, 
SHIPPINGPARTNRADDRESS.CityName as SHIPTOCITY,
SHIPPINGPARTNRADDRESS.PostalCode as SHIPTOPO,


transporterPARTNR.AddressID as transpoter1,
transporterPARTNR.Customer as transpotercustomer,

case when trans.Supplier <> ''
then trans.Supplier else 
transporterPARTNR.Supplier
end as transpotersupplier,

//transporterPARTNR.Supplier as transpotersupplier,

case when trans.TaxNumber3 <> ''
then trans.TaxNumber3 else 
transporterPARTNRADDRESS.TaxNumber3
end as TRANSID,

//transporterPARTNRADDRESS.TaxNumber3 as TRANSID,

case when trans.SupplierName <> ''
then trans.SupplierName else 
transporterPARTNRADDRESS.SupplierName 
end as TRANSPORTERNAME,
//transporterPARTNRADDRESS.Supplier   as TransDocNo ,

DOCHEAD.YY1_LRNO1_BDH  as TransDocNo ,
transporterPARTNRADDRESS.Region as Transporter_State_Code,
transporterPARTNRADDRESS.TaxNumber4 as VEHICLENUMBER2 , 



@Semantics.amount.currencyCode: 'TransactionCurrency'
case when a.BillingDocumentType = 'F8' or a.BillingDocumentType = 'JSN' or a.BillingDocumentType = 'JSTO' then
PCIP.ConditionAmount else cast( 0 as abap.curr( 15, 2 )) end as PCIP_AMT1,
PCIP.ConditionType as PCIP_COND1,

case when a.BillingDocumentType = 'F8' or a.BillingDocumentType = 'JSN' or a.BillingDocumentType = 'JSTO' then
PCIP.ConditionRateAmount else  cast( 0 as abap.dec( 24, 9 )) end  as PCIP_RATE,


ZR00.ConditionType as CONDTYPE ,
ZR00.ConditionCurrency as CURRENCY,
ZR00.ConditionRateAmount as BASICRATE,


@Semantics.amount.currencyCode: 'TransactionCurrency'
ZR00.ConditionAmount as Basic_Amount ,

JOIG.ConditionType as TAXCOND1,
@Semantics.amount.currencyCode: 'TransactionCurrency'
JOIG.ConditionRateRatio as IGSTRATE,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
JOIG.ConditionAmount as IGST,
JOCG.ConditionType as TAXCOND2,
@Semantics.amount.currencyCode: 'TransactionCurrency' 
JOCG.ConditionRateRatio as CGSTRATE,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
JOCG.ConditionAmount as CGST,
JOSG.ConditionType as TAXCOND3,
@Semantics.amount.currencyCode: 'TransactionCurrency'
JOSG.ConditionRateRatio as SGSTRATE,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
JOSG.ConditionAmount as SGST,

@Semantics.amount.currencyCode: 'TransactionCurrency'
ZTCS.ConditionRateRatio as ZTCSRATE,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
ZTCS.ConditionAmount as ZTCS_AMT,

//@Semantics.amount.currencyCode: 'TransactionCurrency'
//D100.ConditionRateAmount as D100_AMT ,

@Semantics.amount.currencyCode: 'TransactionCurrency'
ZDIS.ConditionRateRatio as DISRATE,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
ZDIS.ConditionAmount as DIS_AMT,

@Semantics.amount.currencyCode: 'TransactionCurrency'
ZFO1.ConditionRateRatio as ZFO1RATE,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
ZFO1.ConditionAmount as ZFO1_AMT,

@Semantics.amount.currencyCode: 'TransactionCurrency'
ZSHE.ConditionRateRatio as SHERATE,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
ZSHE.ConditionAmount as SHE_AMT,

@Semantics.amount.currencyCode: 'TransactionCurrency'
ZDIH.ConditionRateRatio as ZDIHRATE,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
ZDIH.ConditionAmount as ZDIH_AMT,

@Semantics.amount.currencyCode: 'TransactionCurrency'
ZDOF.ConditionRateRatio as ZDOFRATE,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
ZDOF.ConditionAmount as ZDOF_AMT,

ZCDI.ConditionRateRatio as ZCDIRATE,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
ZCDI.ConditionAmount as ZCDI_AMT,
@Semantics.amount.currencyCode: 'TransactionCurrency'

ZCDE.ConditionAmount as ZCDE_AMT,
@Semantics.amount.currencyCode: 'TransactionCurrency'

ZCDE.ConditionRateRatio as ZCDERATE,
      @Semantics.amount.currencyCode: 'TransactionCurrency'


ZDQT.ConditionRateValue as ZDQTRATE,
      @Semantics.amount.currencyCode: 'TransactionCurrency'
ZDQT.ConditionAmount as ZDQT_AMT,

//ZLDA','ZPCA','JTC1','JTC2'
@Semantics.amount.currencyCode: 'TransactionCurrency'
( JOCG.ConditionAmount  + JOSG.ConditionAmount + JOIG.ConditionAmount ) as totalGST,

DOCHEAD.BillingDocumentDate,
DOCHEAD.CreationTime,
DOCHEAD.BillingDocumentType,
DOCHEAD.BillingDocumentIsCancelled,
DOCHEAD.CreatedByUser,
DOCHEAD.CancelledBillingDocument,
DOCHEAD.TotalNetAmount as docuhead_netamt,
DOCHEAD.TotalTaxAmount as docuhead_total_tax,
DOCHEAD.IncotermsClassification,
DOCHEAD.PaymentMethod,
DOCHEAD.SDPricingProcedure,
DOCHEAD.CustomerPaymentTerms,
DOCHEAD.IncotermsTransferLocation,
DOCHEAD.YY1_VehicleNo_BDH as VehicalNo,
DOCHEAD.YY1_DriverDetails_BDH,
DOCHEAD.YY1_VehicleNo_BDH,
DOCHEAD.YY1_LRDate_BDH,
DOCHEAD.YY1_LRNO1_BDH,
DOCHEAD.YY1_VehicleNo_BDH                                                               as VEHICLENUMBER,
IRNDETALS.Irn ,
IRNDETALS.IrnStatus ,
IRNDETALS.AckNo ,
IRNDETALS.AckDate,
IRNDETALS.SignedQrcode,
IRNDETALS.Distance as Distance2,
EWAYBILL.Ebillno,
EWAYBILL.EgenDat,
EWAYBILL.Status,
EWAYBILL.Distance ,
EWAYBILL.Vdtodate,
EWAYBILL.validupto,
EWAYBILL.transporter as trans,
      DELIVERYDATA.DeliveryDate,
      DELIVERYDATA.ShippingType,       
      DELIVERYDATA.ActualGoodsMovementTime,
      DELIVERYDATA.ActualGoodsMovementDate

}


