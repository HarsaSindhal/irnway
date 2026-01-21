@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'YJ1IG_EWAYBILL DATA DEFINATION'
define root view entity YJ1IG_EWAYBILLDD as select from yj1ig_ewaybill
 {
    key bukrs as Bukrs,
    key doctyp as Doctyp,
    key docno as Docno,
    key gjahr as Gjahr,
    key ebillno as Ebillno,
    egen_dat as EgenDat,
    vdfmdate as Vdfmdate,
    vdtodate as Vdtodate,
    vdfmtime as Vdfmtime,
    vdtotime as Vdtotime,
    status as Status,
    ernam as Ernam,
    erdat as Erdat,
    aenam as Aenam,
    aedat as Aedat,
    distance as Distance,
    vehiclenum as vehiclenum,
  ewbnumber  ,
  updateddate ,
  transporter,
  vdtodate as validupto   
}
