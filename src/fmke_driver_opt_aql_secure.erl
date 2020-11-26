%% --------------------------------------------------------------
%% Database driver for AQL, an SQL-like interface for AntidoteDB.
%% --------------------------------------------------------------

-module(fmke_driver_opt_aql_secure).

-behaviour(gen_server).

-include("fmke.hrl").
-include("fmke_kv.hrl").

%% gen_server exports
-export([
    start_link/1,
    stop/1,
    init/1,
    handle_call/3,
    handle_cast/2
]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    gen_server:stop(Pid).

init([]) ->
    {ok, _Started} = application:ensure_all_started(aqlc),
    {ok, {1}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({Op, Client}, {EntityID}) ->
    {Reply, NewEntityID} = call(Op, EntityID),
    gen_server:reply(Client, Reply),
    poolboy:checkin(handlers, self()),
    {noreply, {NewEntityID}}.

call({read, patient, Id}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_patient(Conn, Id) of
        {ok, {Id, Name, Address}} ->
            Prescriptions = case read_pat_presc_ids(Conn, Id) of
                {ok, P} when is_list(P) ->
                    P;
                _Error ->
                    []
            end,
            {#patient{id = Id, name = Name, address = Address, prescriptions = Prescriptions}, EntityID};
        Error ->
            {Error, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({read, pharmacy, Id}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_pharmacy(Conn, Id) of
        {ok, {Id, Name, Address}} ->
            Prescriptions = case read_pharm_presc_ids(Conn, Id) of
                {ok, P} when is_list(P) ->
                    P;
                _Error ->
                    []
            end,
            {#pharmacy{id = Id, name = Name, address = Address, prescriptions = Prescriptions}, EntityID};
        Error ->
            {Error, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({read, facility, Id}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_facility(Conn, Id) of
        {ok, {Id, Name, Address, Type}} ->
            {#facility{id = Id, name = Name, address = Address, type = Type}, EntityID};
        Error ->
            {Error, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({read, staff, Id}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_staff(Conn, Id) of
        {ok, {Id, Name, Address, Speciality}} ->
            Prescriptions = case read_staff_presc_ids(Conn, Id) of
                {ok, P} when is_list(P) ->
                    P;
                _Error ->
                    []
            end,
            {#staff{id = Id, name = Name, address = Address, speciality = Speciality, prescriptions = Prescriptions}, EntityID};
        Error ->
            {Error, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({create, facility, [Id, Name, Address, Type]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_facility(Conn, Id) of
        {error, not_found} ->
            {create_facility(Conn, Id, Name, Address, Type), EntityID};
        {ok, _Facility} ->
            {{error, id_taken(facility)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({update, facility, [Id, Name, Address, Type]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_facility(Conn, Id) of
        {error, not_found} ->
            {{error, no_such_entity(facility)}, EntityID};
        {ok, _Facility} ->
            {update_facility(Conn, Id, Name, Address, Type), EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({create, patient, [Id, Name, Address]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_patient(Conn, Id) of
        {error, not_found} ->
            {create_patient(Conn, Id, Name, Address), EntityID};
        {ok, _Patient} ->
            {{error, id_taken(patient)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({update, patient, [Id, Name, Address]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_patient(Conn, Id) of
        {error, not_found} ->
            {{error, no_such_entity(patient)}, EntityID};
        {ok, _Patient} ->
            {update_patient(Conn, Id, Name, Address), EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({create, pharmacy, [Id, Name, Address]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_pharmacy(Conn, Id) of
        {error, not_found} ->
            {create_pharmacy(Conn, Id, Name, Address), EntityID};
        {ok, _Pharmacy} ->
            {{error, id_taken(pharmacy)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({update, pharmacy, [Id, Name, Address]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_pharmacy(Conn, Id) of
        {error, not_found} ->
            {{error, no_such_entity(pharmacy)}, EntityID};
        {ok, _Pharmacy} ->
            {update_pharmacy(Conn, Id, Name, Address), EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({create, staff, [Id, Name, Address, Speciality]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_staff(Conn, Id) of
        {error, not_found} ->
            {create_staff(Conn, Id, Name, Address, Speciality), EntityID};
        {ok, _Staff} ->
            {{error, id_taken(staff)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({update, staff, [Id, Name, Address, Speciality]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_staff(Conn, Id) of
        {error, not_found} ->
            {{error, no_such_entity(staff)}, EntityID};
        {ok, _Staff} ->
            {update_staff(Conn, Id, Name, Address, Speciality), EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({read, staff, Id, prescriptions}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_staff(Conn, Id) of
        {error, not_found} ->
            {{error, no_such_entity(staff)}, EntityID};
        {ok, _Staff} ->
            case read_staff_presc_ids(Conn, Id) of
                {ok, P} when is_list(P) ->
                    Prescriptions = lists:map(fun(PrescriptionId) ->
                        {ok, {_, PatID, DocID, PharmID, DatePrescribed, DateProcessed}} =
                            read_prescription(Conn, PrescriptionId),
                        {ok, Drugs} = read_presc_drugs(Conn, PrescriptionId),
                        make_prescription(PrescriptionId, PatID, DocID, PharmID, DatePrescribed, DateProcessed, Drugs)
                    end, P),
                    {Prescriptions, EntityID};
                _Error ->
                    {[], EntityID}
            end;
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({read, pharmacy, Id, prescriptions}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_pharmacy(Conn, Id) of
        {error, not_found} ->
            {{error, no_such_entity(pharmacy)}, EntityID};
        {ok, _Pharmacy} ->
            case read_pharm_presc_ids(Conn, Id) of
                {ok, P} when is_list(P) ->
                    Prescriptions = lists:map(fun(PrescriptionId) ->
                        {ok, {_, PatID, DocID, PharmID, DatePrescribed, DateProcessed}} =
                            read_prescription(Conn, PrescriptionId),
                        {ok, Drugs} = read_presc_drugs(Conn, PrescriptionId),
                        make_prescription(PrescriptionId, PatID, DocID, PharmID, DatePrescribed, DateProcessed, Drugs)
                    end, P),
                    {Prescriptions, EntityID};
                _Error ->
                    {[], EntityID}
            end
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({read, pharmacy, Id, processed_prescriptions}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_pharmacy(Conn, Id) of
        {error, not_found} ->
            {{error, no_such_entity(pharmacy)}, EntityID};
        {ok, _Pharmacy} ->
            case read_pharm_presc_ids(Conn, Id) of
                {ok, P} when is_list(P) ->
                    AllPrescriptions = lists:map(fun(PrescriptionId) ->
                        {ok, {_, PatID, DocID, PharmID, DatePrescribed, DateProcessed}} =
                            read_prescription(Conn, PrescriptionId),
                        {ok, Drugs} = read_presc_drugs(Conn, PrescriptionId),
                        make_prescription(PrescriptionId, PatID, DocID, PharmID, DatePrescribed, DateProcessed, Drugs)
                    end, P),
                    ProcessedPrescriptions = lists:filter(fun(Presc) ->
                        Presc#prescription.is_processed == ?PRESCRIPTION_PROCESSED_VALUE
                    end, AllPrescriptions),
                    {ProcessedPrescriptions, EntityID};
                _Error ->
                    {[], EntityID}
            end
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({read, prescription, Id}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_prescription(Conn, Id) of
        {error, not_found} ->
            {{error, no_such_entity(prescription)}, EntityID};
        {ok, {_, PatID, DocID, PharmID, DatePrescribed, DateProcessed}} ->
            case read_presc_drugs(Conn, Id) of
                {ok, Drugs} when is_list(Drugs) ->
                    {make_prescription(Id, PatID, DocID, PharmID, DatePrescribed, DateProcessed, Drugs), EntityID};
                {error, Reason} ->
                    {{error, Reason}, EntityID}
            end;
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({read, prescription, Id, [drugs]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_prescription(Conn, Id) of
        {error, not_found} ->
            {{error, no_such_entity(prescription)}, EntityID};
        {ok, _Prescription} ->
            case read_presc_drugs(Conn, Id) of
                {ok, Drugs} when is_list(Drugs) ->
                    {Drugs, EntityID};
                {error, Reason} ->
                    {{error, Reason}, EntityID}
            end;
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({update, prescription, Id, {date_processed, NewDateProcessed}}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_prescription(Conn, Id) of
        {error, not_found} ->
            {{error, no_such_entity(prescription)}, EntityID};
        {ok, {_, PatID, DocID, PharmID, DatePrescribed, DateProcessed}} ->
            Prescription = make_prescription(Id, PatID, DocID, PharmID, DatePrescribed, DateProcessed, []),
            case Prescription#prescription.is_processed of
                ?PRESCRIPTION_PROCESSED_VALUE ->
                    {{error, prescription_already_processed}, EntityID};
                ?PRESCRIPTION_NOT_PROCESSED_VALUE ->
                    {process_prescription(Conn, Id, NewDateProcessed), EntityID}
            end;
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({update, prescription, Id, {drugs, add, Drugs}}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    Result = case read_prescription(Conn, Id) of
        {error, not_found} ->
            {{error, no_such_entity(prescription)}, EntityID};
        {ok, {_, PatID, DocID, PharmID, DatePrescribed, DateProcessed}} ->
            Prescription = make_prescription(Id, PatID, DocID, PharmID, DatePrescribed, DateProcessed, []),
            case Prescription#prescription.is_processed of 
                ?PRESCRIPTION_PROCESSED_VALUE ->
                    {{error, prescription_already_processed}, EntityID};
                ?PRESCRIPTION_NOT_PROCESSED_VALUE ->
                    NewEntityID = lists:foldl(fun(Drug, NewID) ->
                        create_presc_drugs(Conn, NewID, Id, Drug),
                        NewID + 1
                    end, EntityID, Drugs),
                    {ok, NewEntityID}
            end;
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call({create, prescription, [Id, PatientId, PrescriberId, PharmacyId, DatePrescribed, Drugs]}, EntityID) ->
    Conn = fmke_db_conn_manager:checkout(),
    CheckResult = check_keys(
        Conn,
        [
            {prescription, fun read_prescription/2, Id}
        ],
        [
            {patient, fun read_patient/2, PatientId},
            {pharmacy, fun read_pharmacy/2, PharmacyId},
            {staff, fun read_staff/2, PrescriberId}
        ]
    ),

    Result = case CheckResult of
        ok ->
            create_prescription(Conn, Id, PatientId, PrescriberId, PharmacyId, DatePrescribed),
            
            create_pat_presc(Conn, EntityID, PatientId, Id),
            create_pharm_presc(Conn, EntityID + 1, PharmacyId, Id),
            create_staff_presc(Conn, EntityID + 2, PrescriberId, Id),
            
            NewEntityID = lists:foldl(fun(Drug, NewID) ->
                create_presc_drugs(Conn, NewID, Id, Drug),
                NewID + 1
            end, EntityID + 3, Drugs),

            {ok, NewEntityID};
        {exists, Entity} ->
            {{error, id_taken(Entity)}, EntityID};
        {missing, Entity} ->
            {{error, no_such_entity(Entity)}, EntityID};
        {error, Reason} ->
            {{error, Reason}, EntityID}
    end,
    fmke_db_conn_manager:checkin(Conn),
    Result;

call(_, _) ->
    throw(not_implemented).

%% Queries

read_patient(Conn, ID) ->
    Query = io_lib:format(
        "select ID,Name,Address from FmkePatients where ID = ~B;",
        [ID]
    ),
    case aqlc:query(Conn, Query) of
        {ok, [[]]} ->
            {error, not_found};
        {ok, [[Patient]]} when is_list(Patient) ->
            {ok, {ID, proplists:get_value('Name', Patient), proplists:get_value('Address', Patient)}};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_pat_presc_ids(Conn, PatientID) ->
    Query = io_lib:format(
        "select PrescriptionID from FmkePatientPrescriptions where PatientID = ~B;",
        [PatientID]
    ),
    case aqlc:query(Conn, Query) of
        {ok, [[]]} ->
            {ok, []};
        {ok, [Prescriptions]} when is_list(Prescriptions) ->
            P = lists:map(fun(Prescription) -> proplists:get_value('PrescriptionID', Prescription) end, Prescriptions),
            {ok, P};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_pharmacy(Conn, ID) ->
    Query = io_lib:format(
        "select ID,Name,Address from FmkePharmacies where ID = ~B;",
        [ID]
    ),
    case aqlc:query(Conn, Query) of
        {ok, [[]]} ->
            {error, not_found};
        {ok, [[Pharmacy]]} when is_list(Pharmacy) ->
            {ok, {ID, proplists:get_value('Name', Pharmacy), proplists:get_value('Address', Pharmacy)}};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_pharm_presc_ids(Conn, PharmacyID) ->
    Query = io_lib:format(
        "select PrescriptionID from FmkePharmacyPrescriptions where PharmacyID = ~B",
        [PharmacyID]
    ),
    case aqlc:query(Conn, Query) of
        {ok, [[]]} ->
            {ok, []};
        {ok, [Prescriptions]} when is_list(Prescriptions) ->
            P = lists:map(fun(Prescription) -> proplists:get_value('PrescriptionID', Prescription) end, Prescriptions),
            {ok, P};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_facility(Conn, ID) ->
    Query = io_lib:format(
        "select ID,Name,Address,Type from FmkeTreatmentFacilities where ID = ~B",
        [ID]
    ),
    case aqlc:query(Conn, Query) of
        {ok, [[]]} ->
            {error, not_found};
        {ok, [[Facility]]} when is_list(Facility) ->
            Name = proplists:get_value('Name', Facility),
            Address = proplists:get_value('Address', Facility),
            Type = proplists:get_value('Type', Facility),
            {ok, {ID, Name, Address, Type}};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_prescription(Conn, ID) ->
    Query = io_lib:format(
        "select ID,PatID,DocID,PharmID,DatePrescribed,DateProcessed from FmkePrescriptions where ID = ~B",
        [ID]
    ),
    case aqlc:query(Conn, Query) of
        {ok, [[]]} ->
            {error, not_found};
        {ok, [[Prescription]]} when is_list(Prescription) ->
            PatID = proplists:get_value('PatID', Prescription),
            DocID = proplists:get_value('DocID', Prescription),
            PharmID = proplists:get_value('PharmID', Prescription),
            DatePrescribed = proplists:get_value('DatePrescribed', Prescription),
            DateProcessed = proplists:get_value('DateProcessed', Prescription),
            {ok, {ID, PatID, DocID, PharmID, DatePrescribed, DateProcessed}};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_presc_drugs(Conn, PrescriptionID) ->
    Query = io_lib:format(
        "select Drug from FmkePrescriptionDrugs where PrescriptionID = ~B",
        [PrescriptionID]
    ),
    case aqlc:query(Conn, Query) of
        {ok, [[]]} ->
            {ok, []};
        {ok, [Drugs]} when is_list(Drugs) ->
            D = lists:map(fun(Drug) -> proplists:get_value('Drug', Drug) end, Drugs),
            {ok, D};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_staff(Conn, ID) ->
    Query = io_lib:format(
        "select ID,Name,Address,Speciality from FmkeMedicalStaff where ID = ~B",
        [ID]
    ),
    case aqlc:query(Conn, Query) of
        {ok, [[]]} ->
            {error, not_found};
        {ok, [[Staff]]} when is_list(Staff) ->
            Name = proplists:get_value('Name', Staff),
            Address = proplists:get_value('Address', Staff),
            Speciality = proplists:get_value('Speciality', Staff),
            {ok, {ID, Name, Address, Speciality}};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

read_staff_presc_ids(Conn, StaffID) ->
    Query = io_lib:format(
        "select PrescriptionID from FmkeStaffPrescriptions where StaffID = ~B",
        [StaffID]
    ),
    case aqlc:query(Conn, Query) of
        {ok, [[]]} ->
            {ok, []};
        {ok, [Prescriptions]} when is_list(Prescriptions) ->
            P = lists:map(fun(Prescription) -> proplists:get_value('PrescriptionID', Prescription) end, Prescriptions),
            {ok, P};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, query_failed}
    end.

%% Creates

create_facility(Conn, ID, Name, Address, Type) ->
    Query = io_lib:format(
        "insert into FmkeTreatmentFacilities (ID,Name,Address,Type) VALUES (~B,'~s','~s','~s')",
        [ID, Name, Address, Type]
    ),
    aqlc:query(Conn, Query).

create_patient(Conn, ID, Name, Address) ->
    Query = io_lib:format(
        "insert into FmkePatients (ID,Name,Address) VALUES (~B,'~s','~s')",
        [ID, Name, Address]
    ),
    aqlc:query(Conn, Query).

create_pharmacy(Conn, ID, Name, Address) ->
    Query = io_lib:format(
        "insert into FmkePharmacies (ID,Name,Address) VALUES (~B,'~s','~s')",
        [ID, Name, Address]
    ),
    aqlc:query(Conn, Query).

create_staff(Conn, ID, Name, Address, Speciality) ->
    Query = io_lib:format(
        "insert into FmkeMedicalStaff (ID,Name,Address,Speciality) VALUES (~B,'~s','~s','~s')",
        [ID,Name,Address,Speciality]
    ),
    aqlc:query(Conn, Query).

create_prescription(Conn, ID, PatID, DocID, PharmID, DatePrescribed) ->
    Query = io_lib:format(
        "insert into FmkePrescriptions (ID,PatID,DocID,PharmID,DatePrescribed,DateProcessed) VALUES (~B,~B,~B,~B,'~s','')",
        [ID,PatID,DocID,PharmID,DatePrescribed]
    ),
    aqlc:query(Conn, Query).

create_presc_drugs(Conn, ID, PrescriptionID, Drug) ->
    Query = io_lib:format(
        "insert into FmkePrescriptionDrugs (PrescriptionID,Drug) VALUES (~B,~B,'~s')",
        [ID,PrescriptionID,Drug]
    ),
    aqlc:query(Conn, Query).

create_pat_presc(Conn, ID, PatientID, PrescriptionID) ->
    Query = io_lib:format(
        "insert into FmkePatientPrescriptions (PatientID,PrescriptionID) VALUES (~B,~B,~B)",
        [ID,PatientID,PrescriptionID]
    ),
    aqlc:query(Conn, Query).

create_pharm_presc(Conn, ID, PharmacyID, PrescriptionID) ->
    Query = io_lib:format(
        "insert into FmkePharmacyPrescriptions (PharmacyID,PrescriptionID) VALUES (~B,~B,~B)",
        [ID,PharmacyID,PrescriptionID]
    ),
    aqlc:query(Conn, Query).

create_staff_presc(Conn, ID, StaffID, PrescriptionID) ->
    Query = io_lib:format(
        "insert into FmkeStaffPrescriptions (StaffID,PrescriptionID) VALUES (~B,~B,~B)",
        [ID,StaffID,PrescriptionID]
    ),
    aqlc:query(Conn, Query).

%% Updates

update_facility(Conn, ID, Name, Address, Type) ->
    Query = io_lib:format(
        "update FmkeTreatmentFacilities set Name = '~s', Address = '~s', Type = '~s' where ID = ~B",
        [Name, Address, Type, ID]
    ),
    aqlc:query(Conn, Query).

update_patient(Conn, ID, Name, Address) ->
    Query = io_lib:format(
        "update FmkePatients set Name = '~s', Address = '~s' where ID = ~B",
        [Name, Address, ID]
    ),
    aqlc:query(Conn, Query).

update_pharmacy(Conn, ID, Name, Address) ->
    Query = io_lib:format(
        "update FmkePharmacies set Name = '~s', Address = '~s' where ID = ~B",
        [Name, Address, ID]
    ),
    aqlc:query(Conn, Query).

update_staff(Conn, ID, Name, Address, Speciality) ->
    Query = io_lib:format(
        "update FmkeMedicalStaff set Name = '~s', Address = '~s', Speciality = '~s' where ID = ~B",
        [Name, Address, Speciality, ID]
    ),
    aqlc:query(Conn, Query).

process_prescription(Conn, ID, DateProcessed) ->
    Query = io_lib:format(
        "update FmkePrescriptions set DateProcessed = '~s' where ID = ~B",
        [DateProcessed, ID]
    ),
    aqlc:query(Conn, Query).

no_such_entity(facility) ->     no_such_facility;
no_such_entity(patient) ->      no_such_patient;
no_such_entity(pharmacy) ->     no_such_pharmacy;
no_such_entity(prescription) -> no_such_prescription;
no_such_entity(staff) ->        no_such_staff.

id_taken(facility) ->     facility_id_taken;
id_taken(patient) ->      patient_id_taken;
id_taken(pharmacy) ->     pharmacy_id_taken;
id_taken(prescription) -> prescription_id_taken;
id_taken(staff) ->        staff_id_taken.

make_prescription(ID, PatID, DocID, PharmID, DatePrescribed, DateProcessed, Drugs) ->
    #prescription{
        id = ID,
        patient_id = PatID,
        pharmacy_id = PharmID,
        prescriber_id = DocID,
        date_prescribed = DatePrescribed,
        date_processed = case DateProcessed of
            "" ->
                <<"undefined">>;
            Date ->
                Date
        end,
        drugs = Drugs,
        is_processed = case DateProcessed of
            "" ->
                ?PRESCRIPTION_NOT_PROCESSED_VALUE;
            _Other ->
                ?PRESCRIPTION_PROCESSED_VALUE
        end
    }.

check_keys(_, [], []) ->
    ok;
check_keys(Conn, [], [{Entity, Fn, Id} | Rest]) ->
    case Fn(Conn, Id) of
        {error, not_found} ->
            {missing, Entity};
        {ok, _} ->
            check_keys(Conn, [], Rest);
        {error, Reason} ->
            {error, Reason}
    end;
check_keys(Conn, [{Entity, Fn, Id} | Rest], ShouldExist) ->
    case Fn(Conn, Id) of
        {error, not_found} ->
            check_keys(Conn, Rest, ShouldExist);
        {ok, _} ->
            {exists, Entity};
        {error, Reason} ->
            {error, Reason}
    end.
