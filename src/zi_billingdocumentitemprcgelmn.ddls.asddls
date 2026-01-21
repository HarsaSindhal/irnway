@AbapCatalog.sqlViewName: 'ZI_BILLINGDO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'I_BillingDocumentItemPrcgElmnt'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_BillingDocumentItemPrcgElmn as select from I_BillingDocumentItemPrcgElmnt
{
    key BillingDocument,
    key BillingDocumentItem,
    sum( ConditionAmount ) as ConditionAmount,
    sum(ConditionRateAmount) as ConditionRateAmount,
    ConditionRateRatio,
    ConditionRateValue,
    ConditionCurrency,
    ConditionInactiveReason,
    ConditionType
} 
 where      ConditionInactiveReason != 'X'
        and ConditionInactiveReason != 'M'
        and ConditionInactiveReason != 'T'
        and ConditionInactiveReason != 'K'
       // and ConditionInactiveReason != 'Y'
group by 
      BillingDocument,
      BillingDocumentItem,
      ConditionRateRatio,
      ConditionRateValue,
      ConditionCurrency,
      ConditionInactiveReason,
      ConditionType
