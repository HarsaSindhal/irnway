@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'IRN Details'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity yirn_det
  as select from    y1ig_invrefnum as a
    left outer join yj1ig_ewaybill as b on  b.bukrs = a.bukrs
                                        and b.docno = a.docno
{
  key a.bukrs         as Bukrs,
  key a.docno         as Docno,
  key a.doc_year      as DocYear,
      a.doc_type      as DocType,
      a.odn           as Odn,
      a.irn           as Irn,
      a.version       as Version,
      a.bupla         as Bupla,
      a.odn_date      as OdnDate,
      a.ack_no        as AckNo,
      a.ack_date      as AckDate,
      a.irn_status    as IrnStatus,
      a.cancel_date   as CancelDate,
      a.ernam         as Ernam,
      a.erdat         as Erdat,
      a.erzet         as Erzet,
      a.signed_inv    as SignedInv,
      a.signed_qrcode as SignedQrcode,
      b.doctyp        as Doctyp,
      b.gjahr         as Gjahr,
      b.ebillno       as Ebillno,
      b.egen_dat      as EgenDat,
      b.vdfmdate      as Vdfmdate,
      b.vdtodate      as Vdtodate,
      b.vdfmtime      as Vdfmtime,
      b.vdtotime      as Vdtotime,
      b.status        as Status,
      b.ernam         as Eway_Ernam,
      b.erdat         as Eway_Erdat,
      b.aenam         as Aenam,
      b.aedat         as Aedat,
      b.distance      as Distance,
      b.vehiclenum    as Vehiclenum
}
where
  a.irn_status = 'ACT'
