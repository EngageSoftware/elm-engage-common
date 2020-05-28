module Engage.Custom.Field.View exposing (Args, checkBoxList, checkbox, class, countryDropdown, datepicker, dropdown, dropdownWithItems, fieldClass, fieldForm, fieldGroupClass, fieldGroupForm, fieldLabelClass, file, membershipTypeList, number, radioList, regionDropdown, staticForm, text, textArea, textBox, view, viewCompletedEntries, viewEntry)

import Date
import Dict
import Engage.CssHelpers
import Engage.Custom.Field as Field exposing (FieldData)
import Engage.Custom.Field.Events as Events
import Engage.Custom.Field.Helpers as FieldHelpers
import Engage.Custom.Types exposing (..)
import Engage.Entity.Address as Address
import Engage.Form.Address as Address
import Engage.Form.MembershipTypeList as MembershipTypeList
import Engage.Html.Extra as HtmlExtra
import Engage.Localization as Localization
import Engage.Namespace as Namespace
import Engage.String
import Engage.UI.Accordion as Accordion
import Engage.UI.Datepicker as Datepicker
import Engage.UI.Dropdown as Dropdown
import Engage.UI.Error as Error
import Engage.UI.Info as Info
import Engage.UI.Input as Input
import Engage.Validation exposing (ValidationErrors)
import Html exposing (Html)
import Markdown
import Set
import String
import Time


class =
    Namespace.engagecore
        |> Namespace.toString
        |> Engage.CssHelpers.withNamespace


fieldGroupClass : Form -> Section -> FieldGroup -> String
fieldGroupClass form section fieldGroup =
    "Form" ++ String.fromInt form.formId ++ "-Section" ++ String.fromInt section.sectionId ++ "-FieldGroup" ++ String.fromInt fieldGroup.fieldGroupId


fieldGroupForm : Args a msg -> ( Form, Section ) -> FieldGroup -> Html msg
fieldGroupForm args ( form, section ) fieldGroup =
    Html.div [ class [ "FieldGroup", fieldGroupClass form section fieldGroup ] ]
        (fieldGroup.fields
            |> Dict.values
            |> List.sortBy .relativeOrder
            |> List.map (fieldForm args ( form, section, fieldGroup ))
        )


fieldClass : Form -> Section -> FieldGroup -> Field -> String
fieldClass form section fieldGroup field =
    "Form" ++ String.fromInt form.formId ++ "-Section" ++ String.fromInt section.sectionId ++ "-FieldGroup" ++ String.fromInt fieldGroup.fieldGroupId ++ "-Field" ++ String.fromInt field.fieldId


fieldLabelClass : Field -> String
fieldLabelClass field =
    "Field-" ++ Engage.String.toSafeCssClassName field.label


fieldForm : Args a msg -> ( Form, Section, FieldGroup ) -> Field -> Html msg
fieldForm args ( form, section, fieldGroup ) field =
    Html.div [ class [ fieldClass form section fieldGroup field, fieldLabelClass field ] ]
        [ case field.fieldType of
            TextBox { state } ->
                textBox
                    args
                    state
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }

            LargeTextBox { state } ->
                textBox
                    args
                    state
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }

            TextArea { state } ->
                textArea
                    args
                    state
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }

            File { state } ->
                file
                    args
                    state
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }

            CheckBox { state } ->
                checkbox
                    args
                    state
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }

            DropDown { state, fieldChoices } ->
                dropdown
                    { config = args.config
                    , validations = args.validations
                    , fieldChoices = fieldChoices
                    }
                    state
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }

            RadioList { state, fieldChoices } ->
                radioList
                    { config = args.config
                    , validations = args.validations
                    , fieldChoices = fieldChoices
                    }
                    state
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }

            CheckBoxList { state, fieldChoices } ->
                checkBoxList
                    { config = args.config
                    , validations = args.validations
                    , fieldChoices = fieldChoices
                    }
                    state
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }

            Quantity { state } ->
                number
                    { config = args.config
                    , validations = args.validations
                    , maxValue = field.valueMax |> String.toInt
                    , minValue = field.valueMin |> String.toInt
                    }
                    state
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }

            Date { state } ->
                datepicker
                    args
                    state
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }

            Email ->
                Html.text "email is not implemented"

            Phone ->
                Html.text "phone is not implemented"

            ZipCode ->
                Html.text "zip code is not implemented"

            USState ->
                Html.text "us state is not implemented"

            Country { state } ->
                countryDropdown
                    args
                    state
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }

            Region { state } ->
                regionDropdown
                    args
                    state
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }

            Text ->
                text field

            StaticForm staticFormType ->
                staticForm
                    args
                    { form = form, section = section, fieldGroup = fieldGroup, field = field }
                    staticFormType
        ]


staticForm : Args a msg -> FieldData -> StaticFormType -> Html msg
staticForm args { form, section, fieldGroup, field } staticFormType =
    case field.disable of
        Hidden ->
            HtmlExtra.none

        Disabled ->
            HtmlExtra.none

        None ->
            case staticFormType of
                ParticipantForm ->
                    Html.text <| "ParticipantForm"

                MembershipTypeList data ->
                    membershipTypeList args { form = form, section = section, fieldGroup = fieldGroup, field = field } data


membershipTypeList : Args a msg -> FieldData -> { membershipTypeList : List MembershipTypeList.MembershipType, state : Accordion.State, entry : Maybe MembershipTypeList.MembershipType } -> Html msg
membershipTypeList args { form, section, fieldGroup, field } membership =
    let
        domId =
            Field.namespacedId field

        onChange : { onlyStateChange : Bool } -> Accordion.State -> Maybe MembershipTypeList.MembershipType -> msg
        onChange { onlyStateChange } accordionState membershipType =
            Events.onMembershipTypeHandler args.config
                { fieldId = field.fieldId
                , formId = form.formId
                , sectionId = section.sectionId
                , fieldGroupId = fieldGroup.fieldGroupId
                , fieldType = FieldHelpers.updateAccordionState accordionState field.fieldType
                , domId = domId
                , onlyStateChange = onlyStateChange
                }
                membershipType

        requiredText =
            if field.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" args.config)

            else
                Nothing

        --args.config.onChange  accordionState membershipType
    in
    MembershipTypeList.form
        { id = domId
        , priceText = Localization.localizeString "Price:" args.config
        , membershipTypeList = membership.membershipTypeList
        , labelText = field.label
        , helpText = field.description
        , requiredText = requiredText
        , onChange = onChange
        , status = FieldHelpers.toError args.validations field
        , accordionExpandButtonText = Localization.localizeString "More Info" args.config
        }
        membership.state
        membership.entry


text : Field -> Html msg
text field =
    let
        defaultOptions =
            Markdown.defaultOptions
    in
    case field.disable of
        Hidden ->
            HtmlExtra.none

        Disabled ->
            HtmlExtra.none

        None ->
            Html.p []
                [ Markdown.toHtml
                    []
                    (if String.isEmpty field.description then
                        field.label

                     else
                        field.description
                    )
                ]


type alias Args a msg =
    { a
        | config : Config msg
        , validations : ValidationErrors { fieldId : Int }
    }


number : Args { a | maxValue : Maybe Int, minValue : Maybe Int } msg -> Input.State -> FieldData -> Html msg
number { config, validations, maxValue, minValue } state { form, section, fieldGroup, field } =
    let
        domId =
            Field.namespacedId field

        onChange { onlyStateChange } inputState value =
            Events.onChangeHandler config
                { fieldId = field.fieldId
                , formId = form.formId
                , sectionId = section.sectionId
                , fieldGroupId = fieldGroup.fieldGroupId
                , fieldType = FieldHelpers.updateInputState inputState field.fieldType
                , domId = domId
                , onlyStateChange = onlyStateChange
                }
                (value |> Maybe.map String.fromInt |> Maybe.withDefault "")

        labelKey =
            section.name ++ "." ++ field.label

        requiredText =
            if field.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" config)

            else
                Nothing
    in
    Input.number
        { id = domId
        , labelText = Localization.localizeStringWithDefault field.label labelKey config
        , helpText = field.description
        , requiredText = requiredText
        , onChange = onChange
        , status = FieldHelpers.toError validations field
        , namespace = Namespace.engagecore
        , maxValue = maxValue
        , minValue = minValue
        }
        state
        (FieldHelpers.getValue field |> Maybe.andThen List.head |> Maybe.andThen String.toInt)


textBox : Args a msg -> Input.State -> FieldData -> Html msg
textBox { config, validations } state { form, section, fieldGroup, field } =
    let
        domId =
            Field.namespacedId field

        onChange { onlyStateChange } inputState value =
            Events.onChangeHandler config
                { fieldId = field.fieldId
                , formId = form.formId
                , sectionId = section.sectionId
                , fieldGroupId = fieldGroup.fieldGroupId
                , fieldType = FieldHelpers.updateInputState inputState field.fieldType
                , domId = domId
                , onlyStateChange = onlyStateChange
                }
                value

        labelKey =
            section.name ++ "." ++ field.label

        requiredText =
            if field.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" config)

            else
                Nothing
    in
    Input.text
        { id = domId
        , labelText = Localization.localizeStringWithDefault field.label labelKey config
        , helpText = field.description
        , requiredText = requiredText
        , onChange = onChange
        , status = FieldHelpers.toError validations field
        , namespace = Namespace.engagecore
        }
        state
        (FieldHelpers.getValue field |> Maybe.andThen List.head |> Maybe.withDefault "")


textArea : Args a msg -> Input.State -> FieldData -> Html msg
textArea { config, validations } state { form, section, fieldGroup, field } =
    let
        domId =
            Field.namespacedId field

        onChange { onlyStateChange } inputState value =
            Events.onChangeHandler config
                { fieldId = field.fieldId
                , formId = form.formId
                , sectionId = section.sectionId
                , fieldGroupId = fieldGroup.fieldGroupId
                , fieldType = FieldHelpers.updateInputState inputState field.fieldType
                , domId = domId
                , onlyStateChange = onlyStateChange
                }
                value

        labelKey =
            section.name ++ "." ++ field.label

        requiredText =
            if field.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" config)

            else
                Nothing
    in
    Input.textArea
        { id = domId
        , labelText = Localization.localizeStringWithDefault field.label labelKey config
        , helpText = field.description
        , requiredText = requiredText
        , onChange = onChange
        , status = FieldHelpers.toError validations field
        , namespace = Namespace.engagecore
        }
        state
        (FieldHelpers.getValue field |> Maybe.andThen List.head |> Maybe.withDefault "")


checkBoxList : Args { fieldChoices : List FieldChoice } msg -> Input.State -> FieldData -> Html msg
checkBoxList { config, validations, fieldChoices } state { form, section, fieldGroup, field } =
    let
        domId =
            Field.namespacedId field

        onChange { onlyStateChange } newState values =
            Events.onMultipleAnswerChangeHandler config
                { fieldId = field.fieldId
                , formId = form.formId
                , sectionId = section.sectionId
                , fieldGroupId = fieldGroup.fieldGroupId
                , fieldType = FieldHelpers.updateInputState newState field.fieldType
                , domId = domId
                , onlyStateChange = onlyStateChange
                }
                values

        labelKey =
            section.name ++ "." ++ field.label

        requiredText =
            if field.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" config)

            else
                Nothing
    in
    Input.checkBoxList
        { labelText = Localization.localizeStringWithDefault field.label labelKey config
        , id = domId
        , helpText = field.description
        , requiredText = requiredText
        , onChange = onChange
        , status = FieldHelpers.toError validations field
        , namespace = Namespace.engagecore
        , items = FieldHelpers.toRadioItems fieldChoices
        }
        state
        (FieldHelpers.getValues field |> Maybe.withDefault Set.empty)


radioList : Args { a | fieldChoices : List FieldChoice } msg -> Input.State -> FieldData -> Html msg
radioList { config, validations, fieldChoices } state { form, section, fieldGroup, field } =
    let
        domId =
            Field.namespacedId field

        onChange { onlyStateChange } newState value =
            Events.onChangeHandler config
                { fieldId = field.fieldId
                , formId = form.formId
                , sectionId = section.sectionId
                , fieldGroupId = fieldGroup.fieldGroupId
                , fieldType = FieldHelpers.updateInputState newState field.fieldType
                , domId = domId
                , onlyStateChange = onlyStateChange
                }
                value

        labelKey =
            section.name ++ "." ++ field.label

        requiredText =
            if field.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" config)

            else
                Nothing
    in
    Input.radioList
        { labelText = Localization.localizeStringWithDefault field.label labelKey config
        , id = domId
        , helpText = field.description
        , requiredText = requiredText
        , onChange = onChange
        , status = FieldHelpers.toError validations field
        , namespace = Namespace.engagecore
        , items = FieldHelpers.toRadioItems fieldChoices
        }
        state
        (FieldHelpers.getValue field |> Maybe.andThen List.head |> Maybe.withDefault "")


checkbox : Args a msg -> Input.State -> FieldData -> Html msg
checkbox { config, validations } state { form, section, fieldGroup, field } =
    let
        domId =
            Field.namespacedId field

        onCheck { onlyStateChange } inputState value =
            Events.onCheckHandler config
                { fieldId = field.fieldId
                , formId = form.formId
                , sectionId = section.sectionId
                , fieldGroupId = fieldGroup.fieldGroupId
                , fieldType = FieldHelpers.updateInputState inputState field.fieldType
                , domId = domId
                , onlyStateChange = onlyStateChange
                }
                value

        labelKey =
            section.name ++ "." ++ field.label

        requiredText =
            if field.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" config)

            else
                Nothing
    in
    Input.checkbox
        { labelText = Localization.localizeStringWithDefault field.label labelKey config
        , helpText = field.description
        , requiredText = requiredText
        , onCheck = onCheck
        , status = FieldHelpers.toError validations field
        , namespace = Namespace.engagecore
        , state = state
        }
        (FieldHelpers.getBoolValue field |> Maybe.withDefault False)


countryDropdown : Args a msg -> Dropdown.State -> FieldData -> Html msg
countryDropdown args state { form, section, fieldGroup, field } =
    dropdownWithItems args
        state
        { form = form, section = section, fieldGroup = fieldGroup, field = field }
        (args.config.countries
            |> Address.countriesToItems
            |> Dict.values
            |> List.sortBy .text
        )


regionDropdown : Args a msg -> Dropdown.State -> FieldData -> Html msg
regionDropdown args state { form, section, fieldGroup, field } =
    let
        maybeSelectedCountry : Maybe Int
        maybeSelectedCountry =
            section.fieldGroups
                |> Dict.values
                |> List.concatMap (Field.allFields form section)
                |> List.map (\fieldData -> fieldData.field)
                |> List.filter FieldHelpers.isCountryField
                |> List.head
                |> Maybe.andThen FieldHelpers.getValue
                |> Maybe.andThen List.head
                |> Maybe.andThen String.toInt
    in
    dropdownWithItems args
        state
        { form = form, section = section, fieldGroup = fieldGroup, field = field }
        (maybeSelectedCountry
            |> Maybe.map (\countryId -> Address.getRegionsForCountry countryId args.config.regions)
            |> Maybe.map (Address.regionsToItems >> Dict.values >> List.sortBy .text)
            |> Maybe.withDefault []
        )


dropdown : Args { a | fieldChoices : List FieldChoice } msg -> Dropdown.State -> FieldData -> Html msg
dropdown args state { form, section, fieldGroup, field } =
    dropdownWithItems args state { form = form, section = section, fieldGroup = fieldGroup, field = field } (FieldHelpers.toDropdownItems args.fieldChoices)


dropdownWithItems : Args a msg -> Dropdown.State -> FieldData -> List Dropdown.Item -> Html msg
dropdownWithItems { config, validations } state { form, section, fieldGroup, field } items =
    let
        domId =
            Field.namespacedId field

        onChange { onlyStateChange } dropdownState value =
            Events.onChangeHandler config
                { fieldId = field.fieldId
                , formId = form.formId
                , sectionId = section.sectionId
                , fieldGroupId = fieldGroup.fieldGroupId
                , fieldType = FieldHelpers.updateDropdownState dropdownState field.fieldType
                , domId = domId
                , onlyStateChange = onlyStateChange
                }
                (Maybe.withDefault "" value)

        labelKey =
            section.name ++ "." ++ field.label

        requiredText =
            if field.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" config)

            else
                Nothing
    in
    Dropdown.dropdown
        { id = domId
        , labelText = Localization.localizeStringWithDefault field.label labelKey config
        , requiredText = requiredText
        , onChange = onChange
        , items = items
        , status = FieldHelpers.toError validations field
        , namespace = Namespace.engagecore
        }
        state
        (FieldHelpers.getValue field |> Maybe.andThen List.head)


viewEntry : { a | config : Config msg, validations : ValidationErrors { fieldId : Int } } -> Field -> Html msg
viewEntry _ field =
    field
        |> FieldHelpers.getValue
        |> Maybe.map (String.join ", " >> Html.text)
        |> Maybe.withDefault HtmlExtra.none


view : Config msg -> FieldGroup -> List (Html msg)
view config { fields } =
    fields
        |> Dict.values
        |> List.sortBy .relativeOrder
        |> List.map
            (\field ->
                Html.div [ class [ fieldLabelClass field ] ]
                    [ Info.info Namespace.engagecore
                        (Info.getLabel field.label)
                        (field |> FieldHelpers.getText config |> Maybe.map (String.join ", ") |> Maybe.withDefault "")
                    ]
            )


viewCompletedEntries : { a | config : Config msg, validations : ValidationErrors { fieldId : Int } } -> FieldGroup -> List (Html msg)
viewCompletedEntries { config } { fields } =
    if Dict.values fields |> List.any FieldHelpers.isFileField then
        fields
            |> Dict.values
            |> List.map (FieldHelpers.getText config >> Maybe.map (String.join ", ") >> Maybe.withDefault "")
            |> List.map (\value -> Html.li [] [ Html.text value ])

    else
        Html.li []
            [ fields
                |> Dict.values
                |> List.map (FieldHelpers.getText config >> Maybe.map (String.join ", ") >> Maybe.withDefault "")
                |> String.join " "
                |> Html.text
            ]
            |> List.singleton


file : Args a msg -> Input.State -> FieldData -> Html msg
file { config, validations } state { form, section, fieldGroup, field } =
    let
        id =
            Field.namespacedId field

        domId =
            Field.namespacedId field

        onChange { onlyStateChange } inputState value =
            Events.onFileSelectHandler config
                { fieldId = field.fieldId
                , formId = form.formId
                , sectionId = section.sectionId
                , fieldGroupId = fieldGroup.fieldGroupId
                , fieldType = FieldHelpers.updateInputState inputState field.fieldType
                , domId = domId
                , onlyStateChange = onlyStateChange
                }
                value

        error fileEntryData =
            case fileEntryData.status of
                Uploaded ->
                    Error.Ok

                NoFile ->
                    FieldHelpers.toError validations field

                Uploading _ ->
                    FieldHelpers.toError validations field

                Error { message } ->
                    Error.Error { reasons = [ message ] }

        requiredText =
            if field.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" config)

            else
                Nothing
    in
    Html.div []
        [ Input.file
            { id = domId
            , labelText = field.label
            , helpText = field.description
            , requiredText = requiredText
            , browseText = Localization.localizeStringWithDefault "Browse" "Browse" config
            , onChange = onChange
            , status = Field.getFileEntryData field |> Maybe.map error |> Maybe.withDefault Error.Unknown
            , namespace = Namespace.engagecore
            }
            state
            (FieldHelpers.getFileInfo field |> Maybe.withDefault { name = "", fileType = "", progressPercentage = Nothing })
        ]


datepicker : Args a msg -> Datepicker.State -> FieldData -> Html msg
datepicker { config, validations } state { form, section, fieldGroup, field } =
    let
        domId =
            Field.namespacedId field

        onChange dateState value =
            Events.onChangeHandler config
                { fieldId = field.fieldId
                , formId = form.formId
                , sectionId = section.sectionId
                , fieldGroupId = fieldGroup.fieldGroupId
                , fieldType = FieldHelpers.updateDateState dateState field.fieldType
                , domId = domId
                , onlyStateChange = False
                }
                (value |> Maybe.map Date.toIsoString |> Maybe.withDefault "")

        onStateChange dateState =
            Events.onChangeHandler config
                { fieldId = field.fieldId
                , formId = form.formId
                , sectionId = section.sectionId
                , fieldGroupId = fieldGroup.fieldGroupId
                , fieldType = FieldHelpers.updateDateState dateState field.fieldType
                , domId = domId
                , onlyStateChange = True
                }
                (FieldHelpers.getValue field |> Maybe.andThen List.head |> Maybe.withDefault "")

        labelKey =
            section.name ++ "." ++ field.label

        requiredText =
            if field.required then
                Just (Localization.localizeStringWithDefault "Required" "Required" config)

            else
                Nothing
    in
    Datepicker.datepicker
        { id = domId
        , labelText = Localization.localizeStringWithDefault field.label labelKey config
        , requiredText = requiredText
        , onChange = onChange
        , onStateChange = onStateChange
        , status = FieldHelpers.toError validations field
        , namespace = Namespace.engagecore
        }
        state
        (FieldHelpers.getValue field
            |> Maybe.andThen List.head
            |> Maybe.andThen String.toInt
            |> Maybe.map Time.millisToPosix
            |> Maybe.map (Date.fromPosix Time.utc)
        )
