@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'IRN DETAILS TABLE'
define root view entity Y1IG_INVREFNUM_DD as select from y1ig_invrefnum
 {
    key bukrs as Bukrs,
    key docno as Docno,
    doc_year as DocYear,
    doc_type as DocType,
    odn as Odn,
    irn as Irn,
    version as Version,
    bupla as Bupla,
    odn_date as OdnDate,
    ack_no as AckNo,
    ack_date as AckDate,
    irn_status as IrnStatus,
    cancel_date as CancelDate,
    ernam as Ernam,
    erdat as Erdat,
    erzet as Erzet,
    signed_inv as SignedInv,
    signed_qrcode as SignedQrcode,
    lr_no as LR_Number ,
    vehno  ,
    distance as Distance

//    _association_name // Make association public
}
