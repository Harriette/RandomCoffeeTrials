function manage_participants_js(ns_prefix) {

  $("#" + ns_prefix + "manage_participants").on("click", ".delete_btn", function() {
    Shiny.setInputValue(ns_prefix + "participant_email_to_delete", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });

}