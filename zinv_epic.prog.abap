*-- Fetch the VIM Header and Item information based on the DP Document
    SELECT head~docid,
           head~bldat,
           head~xblnr,
           head~ebeln,
           head~gross_amount,
           head~pymnt_terms,
           head~due_date,
           item~ebelp,
           item~wrbtr,
           ekko~bedat,
           ekko~ekgrp,
           ekpo~txz01
           FROM /opt/vim_1head AS head
           INNER JOIN /opt/vim_1item AS item
           ON item~docid = head~docid
           INNER JOIN ekko AS ekko
           ON ekko~ebeln = item~ebeln
           INNER JOIN ekpo AS ekpo
           ON ekpo~ebeln = item~ebeln
           AND ekpo~ebelp = item~ebelp
           INTO TABLE @gt_item
           WHERE head~docid = @iv_dp_document_id.
          " AND head~status = @zif_fin_global_constants_03=>gc_num_11.
    IF sy-subrc = 0.
*-- Send the DP Document data to EPIC through Proxy structure
      me->m_trigger_proxy( EXPORTING it_item = gt_item ).
*-- Check if the GT_MESSAGE table has any error while sending the data to the Proxy
      DATA(ls_message) = VALUE #( gt_message[ msgty = zif_global_constant=>gc_symsgty_e ] OPTIONAL ).    "#EC CI_STDSEQ
      IF ls_message IS INITIAL.
*-- Update the data in the custom table ZFINT_DPAPPROVAL
        me->m_update_approval_table( EXPORTING  it_item = gt_item ).
*-- Check if the GT_MESSAGE table has any error while updating table
        ls_message = VALUE #( gt_message[ msgty = zif_global_constant=>gc_symsgty_e ] OPTIONAL ).  "#EC CI_STDSEQ
        IF ls_message IS INITIAL.
*-- Send Email to the Buyer that Third Party Approval Required for DP document.
          me->m_send_mail_to_buyer( EXPORTING it_item = gt_item ).
        ENDIF.
      ENDIF.
      IF gt_message IS NOT INITIAL.
*-- Update the Application Log with the messages captured
        me->m_update_application_log( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
Footer
© 2022 GitHub, Inc.
Footer navigation
Terms
