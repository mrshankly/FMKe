%%%-------------------------------------------------------------------
%%% @author goncalotomas
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Mar 2017 17:58
%%%-------------------------------------------------------------------
-module(gen_fmke_driver).
-author("goncalotomas").

-include("fmk.hrl").

%%-----------------------------------------------------------------------------
%% Transaction Operations
%%-----------------------------------------------------------------------------

-callback start_transaction(Context::context()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback commit_transaction(Context::context()) ->
  {ok | {error, Reason::term()}, Context::context()}.

%%-----------------------------------------------------------------------------
%% Create Operations
%%-----------------------------------------------------------------------------

-callback create_patient(Context::context(), Id::id(), Name::string(), Address::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback create_pharmacy(Context::context(), Id::id(), Name::string(), Address::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback create_facility(Context::context(), Id::id(), Name::string(), Address::string(), Type::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback create_staff(Context::context(), Id::id(), Name::string(), Address::string(), Speciality::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback create_prescription(Context::context(), PrescriptionId::id(), PatientId::id(), PrescriberId::id(),
      PharmacyId::id(), FacilityId::id(), DatePrescribed::string(), Drugs::list(crdt())) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback create_event(EventId::id(), TreatmentId::id(), StaffMemberId::id(), Timestamp::string(),
      Description::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback create_treatment(TreatmentId::id(), PatientId::id(), StaffId::id(), FacilityId::id(),
      DateStarted::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

%%-----------------------------------------------------------------------------
%% Get Operations
%%-----------------------------------------------------------------------------

-callback get_event_by_id(Context::context(), Id::id()) ->
  {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

-callback get_facility_by_id(Context::context(), Id::id()) ->
  {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

-callback get_patient_by_id(Context::context(), Id::id()) ->
  {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

-callback get_pharmacy_by_id(Context::context(), Id::id()) ->
  {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

-callback get_prescription_by_id(Context::context(), Id::id()) ->
  {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

-callback get_staff_by_id(Context::context(), Id::id()) ->
  {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

-callback get_treatment_by_id(Context::context(), Id::id()) ->
  {{ok, Object::crdt()} | {error, Reason::term()}, Context::context()}.

-callback get_facility_treatments(Context::context(), Id::id()) ->
  {{ok, ListObjects::list(crdt())} | {error, Reason::term()}, Context::context()}.

-callback get_processed_pharmacy_prescriptions(Context::context(), Id::id()) ->
  {{ok, ListObjects::list(crdt())} | {error, Reason::term()}, Context::context()}.

-callback get_pharmacy_prescriptions(Context::context(), Id::id()) ->
  {{ok, ListObjects::list(crdt())} | {error, Reason::term()}, Context::context()}.

-callback get_staff_prescriptions(Context::context(), Id::id()) ->
  {{ok, ListObjects::list(crdt())} | {error, Reason::term()}, Context::context()}.

-callback get_staff_treatments(Context::context(), Id::id()) ->
  {{ok, ListObjects::list(crdt())} | {error, Reason::term()}, Context::context()}.

%%-----------------------------------------------------------------------------
%% Update Operations
%%-----------------------------------------------------------------------------

-callback process_prescription(Context::context(), Id::id(), DateProcessed::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback update_patient_details(Context::context(), Id::id(), Name::string(), Address::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback update_pharmacy_details(Context::context(), Id::id(), Name::string(), Address::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback update_facility_details(Context::context(), Id::id(), Name::string(), Address::string(), Type::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback update_staff_details(Context::context(), Id::id(), Name::string(), Address::string(), Speciality::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback update_patient_details(Context::context(), Id::id(), Name::string(), Address::string()) ->
  {ok | {error, Reason::term()}, Context::context()}.

-callback update_prescription_medication(Context::context(), Id::id(), Drugs::list(crdt())) ->
  {ok | {error, Reason::term()}, Context::context()}.
