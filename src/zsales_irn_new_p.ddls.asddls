@EndUserText.label: 'Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity ZSALES_IRN_NEW_P as select from zsales_irn_new
{
    key BillingDocument,
    CreationDate,
    Irn,
    IrnStatus,
    AckNo,
    AckDate,
    SignedQrcode,
    Ebillno,
    EgenDat,
    Status,
    Distance,
    Vdtodate,
    validupto
}
