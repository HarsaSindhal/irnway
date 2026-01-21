@EndUserText.label: 'Irn Report New'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED }
@Metadata.allowExtensions: true
define root view entity zsales_irn_new
 as select from I_BillingDocumentBasic as a 
 left outer join YJ1IG_EWAYBILLDD as EWAYBILL on a.BillingDocument = EWAYBILL.Docno
 left outer join Y1IG_INVREFNUM_DD as IRNDETALS on a.BillingDocument = IRNDETALS.Docno
 left outer join zeway_part_b as EWAYB on a.BillingDocument = EWAYB.docno
  
{
 key  a.BillingDocument,
      a.CreationDate, 
      IRNDETALS.Irn ,
      IRNDETALS.IrnStatus ,
      IRNDETALS.AckNo ,
      IRNDETALS.AckDate,
      IRNDETALS.SignedQrcode,
      EWAYBILL.Ebillno,
      EWAYBILL.EgenDat,
      EWAYBILL.Status,
      EWAYBILL.Distance,
      EWAYBILL.Vdtodate,
      EWAYB.validupto
 }
