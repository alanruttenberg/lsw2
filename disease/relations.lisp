6.2.2 SNOMED CT® Technical Implementation Guide, January 2015 International Release (US English)
http://doc.ihtsdo.org/download/doc_TechnicalImplementationGuide_Current-en-US_INT_20150131.pdf

6.2.2 Attributes Used in SNOMED CT

for the purpose of the pharmaceutical and biologic product boundary and scope work, "decision support" means:
• Interaction checking
• Contraindication checking
• Dosage checking
• Label text for patients
• Allergy / intolerance checking
• Therapeutic duplication


Table 103: Allowed Attributes by Domain Concept Model Guide
|Administration of substance via specific route (procedure)
| Route of administration |
Anatomical structure (body structure)
| Laterality |
| Part of |
￼Clinical finding (finding)
￼￼￼| After |
￼￼￼| Associated morphology |
￼￼￼| Associated with |
￼￼￼| Causative agent |
￼￼￼| Clinical course |
￼￼￼| Due to |
￼￼￼| Episodicity |
￼￼￼| Finding informer |
￼￼￼| Finding method |
￼￼￼| Finding site |
￼￼￼| Has interpretation |
￼￼￼| Interprets |
￼￼￼| Laterality | This attribute is allowed only in close-to-user form. It is used for user-level composition, and is not applied directly as defining attributes in the distributed form (or in normal forms).
￼￼￼| Occurrence |
￼￼￼| Pathological process |
￼￼￼| Severity |
￼￼￼Disorder (finding)
￼￼￼| Has definitional manifestation |
￼Drug delivery device (physical object)
￼￼￼￼￼| Has active ingredient |
￼￼￼| Has dose form |
￼Evaluation procedure (procedure)
￼￼￼| Component |
￼￼￼| Has specimen |
￼￼￼| Measurement method |
￼￼￼| Property |
￼￼￼| Scale type |
￼￼￼| Time aspect |
￼￼Event (event)
￼￼￼| After |
￼￼￼| Associated with |
￼￼￼| Causative agent |
￼￼￼| Due to |
￼￼￼| Occurrence |
￼￼Pharmaceutical / biologic product (product)
￼￼￼￼￼| Has active ingredient |
￼￼￼| Has dose form |
￼Procedure (procedure)
￼￼￼| Access |
￼￼￼| Direct device |
￼￼￼| Direct morphology |
￼￼￼| Direct substance |
￼￼￼| Has focus |
￼￼￼| Has intent |
￼￼￼| Indirect device |
￼￼￼| Indirect morphology |
￼￼￼| Method |
￼￼￼| Priority |
￼￼￼| Procedure device |
￼￼￼| Procedure morphology |
￼￼￼| Procedure site |
￼￼￼| Procedure site - Direct |
￼￼￼| Procedure site - Indirect |
￼￼￼| Recipient category |
￼￼￼| Revision status |
￼￼￼| Using device |
￼￼￼| Using access device |
￼￼￼| Using energy |
￼￼￼| Using substance |
￼￼Situation with explicit context (situation)
￼￼￼￼￼| Subject relationship context |
￼￼￼| Temporal context |


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Table 104: Historical Relationships by Domain

￼￼￼Finding with explicit context (situation) - descendants only
￼| Associated finding |
￼￼￼Finding with explicit context (situation) - self and descendants
￼￼| Finding context |
￼￼￼Procedure with explicit context (situation) - descendants only
￼￼| Associated procedure |
￼￼￼Procedure with explicit context (situation) - self and descendants
￼￼￼| Procedure context |
￼Specimen (specimen)
￼￼￼| Specimen procedure |
￼￼￼| Specimen source identity |
￼￼￼| Specimen source morphology |
￼￼￼| Specimen source topography |
￼￼￼| Specimen substance |
￼￼￼Surgical procedure (procedure)
￼￼￼| Surgical Approach |
￼￼￼￼￼DOMAIN
￼￼￼HISTORICAL REALATIONSHIP
￼￼￼Ambiguous Concept
￼￼MAYBE A
￼￼￼Duplicate Concept
￼￼￼SAME AS
￼Erroneous Concept
￼￼￼REPLACED BY
￼￼￼WAS A
￼￼Inactive reason Not Stated Concept
￼￼￼REPLACED BY
￼￼￼WAS A
￼￼￼Limited Status Concept
￼￼WAS A
￼￼￼Moved From Elsewhere Concept
￼￼MOVED FROM
￼￼￼Moved To Elsewhere Concept
￼￼￼￼MOVED TO
￼￼￼￼￼DOMAIN
￼￼￼￼￼HISTORICAL REALATIONSHIP
￼Outdated Concept
￼￼￼REPLACED BY
￼￼￼WAS A
￼￼￼Pending Move Concept
￼￼￼MOVED TO


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Table 105: Allowable Ranges for Concept Model Attributes

| ACCESS |
￼￼| Surgical access values | 309795001 (<=)(< Q)
￼￼￼| AFTER |
￼￼| Clinical Finding | 404684003 (<<) | Procedure | 71388002 (<<)
￼￼￼| ASSOCIATED FINDING |
￼| Clinical finding | 404684003 (<=)(< Q)

| Event | 272379006 (<=)(< Q)

| Observable entity | 363787002 (< Q only) | Link assertion | 416698001 (< Q only)

| Procedure | 71388002 (< Q only)
￼￼￼| ASSOCIATED MORPHOLOGY |
￼￼￼| Morphologically abnormal structure | 49755003 (<<)
￼￼￼| ASSOCIATED PROCEDURE |
￼￼| Procedure | 71388002 (<=)(< Q)
| Observable entity | 363787002 (< Q only)

￼￼￼| ASSOCIATED WITH |
￼￼￼| Clinical Finding | 404684003 (<<) | Procedure | 71388002 (<<)
| Event | 272379006 (<<)
| Organism | 410607006 (<<)
| Substance | 105590001 (<<)
| Physical object | 260787004 (<<)
| Physical force | 78621006 (<<)
| Pharmaceutical / biologic product | 373873005 (<< Q only)
| SNOMED CT Concept | 138875005 (==)

￼￼￼| CAUSATIVE AGENT |
￼| Organism | 410607006 (<<)
| Substance | 105590001 (<<)
| Physical object | 260787004 (<<)
| Physical force | 78621006 (<<)
| Pharmaceutical / biologic product | 373873005 (<< Q only)
| SNOMED CT Concept | 138875005 (==)

￼￼￼| COMPONENT |
￼￼| Substance | 105590001 (<=)(< Q)
| Observable entity | 363787002 (<=)(< Q) | Cell structure | 4421005 (<=)(< Q)
| Organism | 410607006 (<=)(< Q)

￼￼￼| CLINICAL COURSE |
￼￼| Courses | 288524001 (<=)(< Q)

￼￼￼| DIRECT DEVICE |
￼￼| Device | 49062001 (<<)

￼￼￼| DIRECT MORPHOLOGY |
￼￼￼| Morphologically abnormal structure | 49755003 (<<)

￼￼￼| DIRECT SUBSTANCE |
￼| Substance | 105590001 (<<)
| Pharmaceutical / biologic product | 373873005 (<<)

￼￼￼| DUE TO |
￼￼| Clinical Finding | 404684003 (<=) | Event | 272379006 (<=)

￼￼￼| EPISODICITY |
￼￼| Episodicities | 288526004 (<=)(< Q)

￼￼￼| FINDING CONTEXT |
￼￼￼| Finding context value | 410514004 (<=)(< Q)

￼￼￼| FINDING INFORMER |
￼| Performer of method | 420158005 (<<)
| Subject of record or other provider of history | 419358007 (<<)

￼￼￼| FINDING METHOD |
￼￼￼| Procedure | 71388002 (<=)

￼￼￼| FINDING SITE |
￼| Anatomical or acquired body structure | 442083009 (<<)

￼￼￼| HAS ACTIVE INGREDIENT |
￼￼| Substance | 105590001 (<<)

￼￼￼| HAS DEFINITIONAL MANIFESTATION |
￼￼| Clinical finding | 404684003 (<<)
￼￼￼| HAS DOSE FORM |
￼￼￼￼| Type of drug preparation | 105904009 (<<)
￼￼© 2002-2015 International Health Terminology Standards Development Organisation CVR #: 30363434

￼￼￼| HAS FOCUS |
￼| Clinical finding | 404684003 (<<) | Procedure | 71388002 (<<)

￼￼￼| HAS INTENT |
￼￼| Intents (nature of procedure values) | 363675004 (<=)

￼￼￼| HAS INTERPRETATION |
￼￼| Findings values | 260245000 (<<)

￼￼￼| HAS SPECIMEN |
￼￼| Specimen | 123038009 (<=)(< Q)

￼￼￼| INDIRECT DEVICE |
￼￼| Device | 49062001 (<<)

￼￼￼| INDIRECT MORPHOLOGY |
￼￼￼| Morphologically abnormal structure | 49755003 (<<)

￼￼￼| INTERPRETS |
￼| Observable entity | 363787002 (<<)
| Laboratory procedure | 108252007 (<<) | Evaluation procedure | 386053000 (<<)

￼￼￼| LATERALITY |
￼￼￼| Side | 182353008 (<=)

￼￼￼| MEASUREMENT METHOD |
￼| Laboratory procedure categorized by method | 127789004(<=)

￼￼￼| METHOD |
￼￼| Action | 129264002 (<<)

￼￼￼| OCCURRENCE |
￼￼￼| Periods of life | 282032007 (<)

￼￼￼| PATHOLOGICAL PROCESS |
￼| Autoimmune | 263680009 (==)
| Infectious process | 441862004 (<<)
| Hypersensitivity process | 472963003 (< <)

￼￼￼| PRIORITY |
￼￼| Priorities | 272125009 (<=)(< Q)

￼￼￼| PROCEDURE CONTEXT |
￼￼| Context values for actions | 288532009 (<=)(< Q)

￼￼￼| PROCEDURE DEVICE |
￼￼| Device | 49062001 (<<)

￼￼￼| PROCEDURE MORPHOLOGY |
￼￼| Morphologically abnormal structure | 49755003 (<<)

￼￼￼| Direct morphology |
￼￼| Morphologically abnormal structure | 49755003 (<<)

￼￼￼| Indirect morphology |
￼￼￼| Morphologically abnormal structure | 49755003 (<<)

￼￼￼| PROCEDURE SITE |
￼￼￼| Anatomical or acquired body structure | 442083009 (<<)

￼￼￼| Procedure site - Direct |
￼| Anatomical or acquired body structure | 442083009 (<<)
￼￼￼| Procedure site - Indirect |
￼￼| Anatomical or acquired body structure | 442083009 (<<)

￼￼￼| PROPERTY |

￼￼￼| Property of measurement | 118598001 (<=)(< Q)

￼￼￼| RECIPIENT CATEGORY |
￼| Person | 125676002 (<<)
| Family | 35359004 (<<)
| Community | 133928008 (<<)
| Donor for medical or surgical procedure | 105455006 (<<)
| Group | 389109008 (<<)

￼￼￼| REVISION STATUS |
￼￼| Primary operation | 261424001 (<<)
| Revision - value | 255231005 (<<)
| Part of multistage procedure | 257958009 (<<)

￼￼￼| ROUTE OF ADMINISTRATION |
￼￼￼| Route of administration value | 284009009 (<<)

￼￼￼| SCALE TYPE |
￼| Quantitative | 30766002 (<<)
| Qualitative | 26716007 (<<)
| Ordinal value | 117363000 (<<)
| Ordinal or quantitative value | 117365007 (<<) | Nominal value | 117362005 (<<)
| Narrative value | 117364006 (<<)
| Text value | 117444000 (<<)
￼￼￼| SEVERITY |
￼￼| Severities | 272141005 (<=)(< Q)
￼￼￼| SPECIMEN PROCEDURE |
￼￼￼| Procedure | 71388002 (<)
￼￼￼| SPECIMEN SOURCE IDENTITY |
￼| Person | 125676002 (<<)
| Family | 35359004 (<<)
| Community | 133928008 (<<)
| Device | 49062001 (<<)
| Environment | 276339004 (<<)
￼￼￼| SPECIMEN SOURCE MORPHOLOGY |
￼￼￼￼| Morphologically abnormal structure | 49755003 (<<)
| SPECIMEN SOURCE TOPOGRAPHY |
| Anatomical or acquired body structure | 442083009 (<<)

| SPECIMEN SUBSTANCE |
| Substance | 105590001 (<<)

| SUBJECT RELATIONSHIP CONTEXT |
| Person | 125676002 (<=)(< Q)

| SURGICAL APPROACH |
| Procedural approach | 103379005 (<=)(< Q)

| TEMPORAL CONTEXT |
| Temporal context value | 410510008 (<=)(< Q)

| TIME ASPECT |
| Time frame | 7389001 (<=)(< Q)

| USING ACCESS DEVICE |
| Device | 49062001 (<<)

| USING DEVICE |
| Device | 49062001 (<<)

| USING ENERGY |
| Physical force | 78621006 (<<)

| USING SUBSTANCE |
| Substance | 105590001 (<<)


Table 106: Top Level Concepts

• | Clinical finding |
• | Procedure |
• | Observable entity |
• | Body structure |
• | Organism |
• | Substance |
• | Pharmaceutical / biologic product |
• | Specimen |
• | Special concept |
• | SNOMED CT Model Component |
• | Physical force |
• | Event |
• | Environment or geographical location |
• | Social context |
• | Situation with explicit context |
• | Staging and scales |
• | Physical object |
• | Qualifier value |
• | Record artifact |
