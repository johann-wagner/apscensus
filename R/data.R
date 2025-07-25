#' APS Employee Census 2024 Individual Responses
#'
#' A dataset containing individual responses from the 2024 Australian Public Service (APS) Employee Census.
#' This data provides insights into various aspects of employment within the APS, including
#' basic demographics, workplace experiences, job satisfaction, and future intentions.
#'
#' @format A \code{tibble} with 140,396 rows and 214 columns:
#' \describe{
#'   \item{\code{agency_size}}{Ordered factor. Agency Size: "Small (Less than 250 employees)", "Medium (251 to 1,000 employees)", "Large (1,001 or more employees)".}
#'   \item{\code{gender}}{Ordered factor. How do you describe your gender? "Man or male", "Woman or female", "Non-binary", "I use a different term", "Prefer not to say".}
#'   \item{\code{age_range}}{Ordered factor. How old were you at your last birthday? "Under 40 years", "40-54 years", "55 years or older", "Prefer not to say".}
#'   \item{\code{classification}}{Ordered factor.What is your current, actual classification level? "Trainee/Graduate/Cadet/APS", "EL", "SES".}
#'   \item{\code{q17a}}{Ordered factor. q17a. My job gives me opportunities to utilise my skills, with Likert scale agreement levels.}
#'   \item{\code{q17b}}{Ordered factor. q17b. I am satisfied with the recognition I receive for doing a good job, with Likert scale agreement levels.}
#'   \item{\code{q17c}}{Ordered factor. q17c. I am fairly remunerated (e.g. salary, superannuation) for the work that I do, with Likert scale agreement levels.}
#'   \item{\code{q17d}}{Ordered factor. q17d. I am satisfied with my non-monetary employment conditions (e.g. leave, flexible work arrangements, other benefits), with Likert scale agreement levels.}
#'   \item{\code{q17e}}{Ordered factor. q17e. I am satisfied with the stability and security of my job, with Likert scale agreement levels.}
#'   \item{\code{q17f}}{Ordered factor. q17f. I suggest ideas to improve our way of doing things, with Likert scale agreement levels.}
#'   \item{\code{q17g}}{Ordered factor. q17g. I am happy to go the 'extra mile' at work when required, with Likert scale agreement levels.}
#'   \item{\code{q17h}}{Ordered factor. q17h. Overall, I am satisfied with my job, with Likert scale agreement levels.}
#'   \item{\code{q17i}}{Ordered factor. q17i. I understand how my role contributes to achieving an outcome for the Australian public, with Likert scale agreement levels.}
#'   \item{\code{q17j}}{Ordered factor. q17j. I am confident that if I requested a flexible work arrangement, my request would be given reasonable consideration, with Likert scale agreement levels.}
#'   \item{\code{q17k}}{Ordered factor. q17k. Where appropriate, I am able to take part in decisions that affect my job, with Likert scale agreement levels.}
#'   \item{\code{q17l}}{Ordered factor. q17l. I feel I have the same opportunities as anyone else of my ability or experience, with Likert scale agreement levels.}
#'   \item{\code{q18a}}{Ordered factor. q18a. When changes occur, the impacts are communicated well within my workgroup, with Likert scale agreement levels.}
#'   \item{\code{q18b}}{Ordered factor. q18b. My workgroup has the appropriate skills, capabilities and knowledge to perform well, with Likert scale agreement levels.}
#'   \item{\code{q18c}}{Ordered factor. q18c. The people in my workgroup use time and resources efficiently, with Likert scale agreement levels.}
#'   \item{\code{q18d}}{Ordered factor. q18d. The people in my workgroup demonstrate stewardship, with Likert scale agreement levels.}
#'   \item{\code{q18e}}{Ordered factor. q18e. The people in my workgroup are able to bring up problems and tough issues, with Likert scale agreement levels.}
#'   \item{\code{q18f}}{Ordered factor. q18f. My workgroup considers the people and businesses affected by what we do, with Likert scale agreement levels.}
#'   \item{\code{q18g}}{Ordered factor. q18g. My workgroup has the tools and resources we need to perform well, with Likert scale agreement levels.}
#'   \item{\code{q19_1}}{Character. q19_1. Information and communications technology (ICT).}
#'   \item{\code{q19_2}}{Character. q19_2. Enabling functions (legal, HR, finance, etc.).}
#'   \item{\code{q19_3}}{Character. q19_3. Additional employees.}
#'   \item{\code{q19_4}}{Character. q19_4. Technical expertise/capability.}
#'   \item{\code{q19_5}}{Character. q19_5. Processes/Frameworks.}
#'   \item{\code{q19_6}}{Character. q19_6. Training/Learning and development opportunities.}
#'   \item{\code{q19_7}}{Character. q19_7. Physical equipment (desk, computer, chair, machinery, phone, stationery).}
#'   \item{\code{q19_8}}{Character. q19_8. Other.}
#'   \item{\code{q20a}}{Ordered factor. q20a. My supervisor communicates effectively, with Likert scale agreement levels.}
#'   \item{\code{q20b}}{Ordered factor. q20b. My supervisor engages with staff on how to respond to future challenges, with Likert scale agreement levels.}
#'   \item{\code{q20c}}{Ordered factor. q20c. My supervisor can deliver difficult advice whilst maintaining relationships, with Likert scale agreement levels.}
#'   \item{\code{q20d}}{Ordered factor. q20d. My supervisor encourages my team to regularly review and improve our work, with Likert scale agreement levels.}
#'   \item{\code{q20e}}{Ordered factor. q20e. My supervisor is invested in my development, with Likert scale agreement levels.}
#'   \item{\code{q20f}}{Ordered factor. q20f. My supervisor provides me with helpful feedback to improve my performance, with Likert scale agreement levels.}
#'   \item{\code{q20g}}{Ordered factor. q20g. My supervisor actively ensures that everyone can be included in workplace activities, with Likert scale agreement levels.}
#'   \item{\code{q20h}}{Ordered factor. q20h. My supervisor ensures that my workgroup delivers on what we are responsible for, with Likert scale agreement levels.}
#'   \item{\code{q20i}}{Ordered factor. q20i. My supervisor invites a range of views, including those different to their own, with Likert scale agreement levels.}
#'   \item{\code{q20j}}{Ordered factor. q20j. My supervisor encourages me to take on new tasks and gain experience doing things I've never done before, with Likert scale agreement levels.}
#'   \item{\code{q22a}}{Ordered factor. q22a. My SES manager communicates effectively, with Likert scale agreement levels.}
#'   \item{\code{q22b}}{Ordered factor. q22b. My SES manager ensures that work effort contributes to the strategic direction of the agency and the APS, with Likert scale agreement levels.}
#'   \item{\code{q22c}}{Ordered factor. q22c. My SES manager clearly articulates the direction and priorities for our area, with Likert scale agreement levels.}
#'   \item{\code{q22d}}{Ordered factor. q22d. My SES manager promotes cooperation within and between agencies, with Likert scale agreement levels.}
#'   \item{\code{q22e}}{Ordered factor. q22e. My SES manager encourages innovation and creativity, with Likert scale agreement levels.}
#'   \item{\code{q22f}}{Ordered factor. q22f. My SES manager presents convincing arguments and persuades others towards an outcome, with Likert scale agreement levels.}
#'   \item{\code{q22g}}{Ordered factor. q22g. My SES manager creates an environment that enables us to deliver our best, with Likert scale agreement levels.}
#'   \item{\code{q22h}}{Ordered factor. q22h. My SES manager routinely promotes the use of data and evidence to deliver outcomes, with Likert scale agreement levels.}
#'   \item{\code{q23a}}{Ordered factor. q23a. In my agency, the SES work as a team, with Likert scale agreement levels.}
#'   \item{\code{q23b}}{Ordered factor. q23b. In my agency, the SES clearly articulate the direction and priorities for our agency, with Likert scale agreement levels.}
#'   \item{\code{q24a}}{Ordered factor. q24a. I feel a strong personal attachment to my agency, with Likert scale agreement levels.}
#'   \item{\code{q24b}}{Ordered factor. q24b. I feel a strong personal attachment to the APS, with Likert scale agreement levels.}
#'   \item{\code{q24c}}{Ordered factor. q24c. I am proud to work in my agency, with Likert scale agreement levels.}
#'   \item{\code{q24d}}{Ordered factor. q24d. Internal communication within my agency is effective, with Likert scale agreement levels.}
#'   \item{\code{q24e}}{Ordered factor. q24e. I would recommend my agency as a good place to work, with Likert scale agreement levels.}
#'   \item{\code{q24f}}{Ordered factor. q24f. I believe strongly in the purpose and objectives of my agency, with Likert scale agreement levels.}
#'   \item{\code{q24g}}{Ordered factor. q24g. I believe strongly in the purpose and objectives of the APS, with Likert scale agreement levels.}
#'   \item{\code{q24h}}{Ordered factor. q24h. My agency supports and actively promotes an inclusive workplace culture, with Likert scale agreement levels.}
#'   \item{\code{q24i}}{Ordered factor. q24i. I work beyond what is required in my job to help my agency achieve its objectives, with Likert scale agreement levels.}
#'   \item{\code{q24j}}{Ordered factor. q24j. I feel committed to my agency's goals, with Likert scale agreement levels.}
#'   \item{\code{q24k}}{Ordered factor. q24k. My agency really inspires me to do my best work every day, with Likert scale agreement levels.}
#'   \item{\code{q24l}}{Ordered factor. q24l. Change is managed well in my agency, with Likert scale agreement levels.}
#'   \item{\code{q24m}}{Ordered factor. q24m. The culture in my agency supports people to act with integrity, with Likert scale agreement levels.}
#'   \item{\code{q24n}}{Ordered factor. q24n. I am supported to use my expertise to provide frank and fearless advice, with Likert scale agreement levels.}
#'   \item{\code{q25_1}}{Character. q25_1. Workplace relationships with colleagues.}
#'   \item{\code{q25_2}}{Character. q25_2. Inclusive work environment.}
#'   \item{\code{q25_3}}{Character. q25_3. Quality of leadership (e.g. supportive, clear communication).}
#'   \item{\code{q25_4}}{Character. q25_4. Access to flexible working arrangements.}
#'   \item{\code{q25_5}}{Character. q25_5. Non-monetary employment conditions (e.g. leave, flexible work arrangements, other benefits).}
#'   \item{\code{q25_6}}{Character. q25_6. Remuneration (e.g. salary, superannuation).}
#'   \item{\code{q25_7}}{Character. q25_7. Type/nature of work (e.g. interesting, challenging, specialised, autonomous).}
#'   \item{\code{q25_8}}{Character. q25_8. Job security.}
#'   \item{\code{q25_9}}{Character. q25_9. Location of work.}
#'   \item{\code{q25_10}}{Character. q25_10. Lack of suitable alternative job prospects.}
#'   \item{\code{q25_11}}{Character. q25_11. Career progression opportunities.}
#'   \item{\code{q25_12}}{Character. q25_12. Professional development (e.g. learning new skills or developing current skills).}
#'   \item{\code{q25_13}}{Character. q25_13. Belief in the purpose and objectives of the APS.}
#'   \item{\code{q25_14}}{Character. q25_14. Service to the Australian public.}
#'   \item{\code{q25_15}}{Character. q25_15. There are no reasons for staying.}
#'   \item{\code{q25_16}}{Character. q25_16. Not applicable.}
#'   \item{\code{q25_17}}{Character. q25_17. Other.}
#'   \item{\code{q26}}{Ordered factor. q26. In the last month, please rate your workgroup's overall performance:, with levels: "Excellent", "Very good", "Average", "Below average", "Well below average".}
#'   \item{\code{q27}}{Ordered factor. q27. What best describes your current workload?, with levels: "Well above capacity - too much work", "Slightly above capacity - lots of work to do", "At capacity - about the right amount of work to do", "Slightly below capacity - available for more work", "Well below capacity - not enough work".}
#'   \item{\code{q28a}}{Ordered factor. q28a. Lack of clarity around my role and responsibilities, with "extent" Likert scale levels.}
#'   \item{\code{q28b}}{Ordered factor. q28b. Lack of clarity around priorities, with "extent" Likert scale levels.}
#'   \item{\code{q28c}}{Ordered factor. q28c. Too many competing priorities, with "extent" Likert scale levels.}
#'   \item{\code{q28d}}{Ordered factor. q28d. Administrative processes within my agency, with "extent" Likert scale levels.}
#'   \item{\code{q28e}}{Ordered factor. q28e. The technology within my agency, with "extent" Likert scale levels.}
#'   \item{\code{q28f}}{Ordered factor. q28f. The internal communication within my agency, with "extent" Likert scale levels.}
#'   \item{\code{q28g}}{Ordered factor. q28g. The lack of inclusiveness in my workgroup, with "extent" Likert scale levels.}
#'   \item{\code{q28h}}{Ordered factor. q28h. Multiple layers of decision making within my agency, with "extent" Likert scale levels.}
#'   \item{\code{q28i}}{Ordered factor. q28i. Authority for decision making is at a higher level than required, with "extent" Likert scale levels.}
#'   \item{\code{q28j}}{Ordered factor. q28j. The appetite for risk within my agency, with "extent" Likert scale levels.}
#'   \item{\code{q28k}}{Ordered factor. q28k. Resistance to experimentation with new ideas, with "extent" Likert scale levels.}
#'   \item{\code{q28l}}{Ordered factor. q28l. Flexible work practices are not supported, with "extent" Likert scale levels.}
#'   \item{\code{q28m}}{Ordered factor. q28m. Limited instances of working as one APS, with "extent" Likert scale levels.}
#'   \item{\code{q28n}}{Ordered factor. q28n. The lack of access to learning and development opportunities, with "extent" Likert scale levels.}
#'   \item{\code{q29}}{Character. q29. Moving forward, what is the most important positive initiative you would like to see in your working environment?}
#'   \item{\code{q30}}{Ordered factor. q30. Are there currently skills or capability gaps within your immediate workgroup?, with levels: "Yes", "No", "Not Sure".}
#'   \item{\code{q31_1}}{Character. q31_1. Written communication.}
#'   \item{\code{q31_2}}{Character. q31_2. Oral communication.}
#'   \item{\code{q31_3}}{Character. q31_3. Information and communications technology (ICT) or digital.}
#'   \item{\code{q31_4}}{Character. q31_4. Data.}
#'   \item{\code{q31_5}}{Character. q31_5. Strategic policy.}
#'   \item{\code{q31_6}}{Character. q31_6. Change management.}
#'   \item{\code{q31_7}}{Character. q31_7. Risk management.}
#'   \item{\code{q31_8}}{Character. q31_8. Leadership.}
#'   \item{\code{q31_9}}{Character. q31_9. Human resources.}
#'   \item{\code{q31_10}}{Character. q31_10. Collaboration and stakeholder engagement.}
#'   \item{\code{q31_11}}{Character. q31_11. Creativity and innovation.}
#'   \item{\code{q31_12}}{Character. q31_12. Project and program management.}
#'   \item{\code{q31_13}}{Character. q31_13. Commercial awareness and business acumen.}
#'   \item{\code{q31_14}}{Character. q31_14. Evaluation.}
#'   \item{\code{q31_15}}{Character. q31_15. Other.}
#'   \item{\code{q32}}{Ordered factor. q32. In the last 12 months, the formal learning I have accessed has improved my performance., with Likert scale agreement and "Not applicable" levels.}
#'   \item{\code{q33}}{Ordered factor. q33. To what extent do you agree that your recent performance and development discussions with your supervisor helped improve your performance?, with Likert scale agreement and "Not applicable" levels.}
#'   \item{\code{q34a}}{Ordered factor. q34a. I have unrealistic time pressures, with Likert scale likelihood levels.}
#'   \item{\code{q34b}}{Ordered factor. q34b. I have a choice in deciding how I do my work, with Likert scale likelihood levels.}
#'   \item{\code{q34c}}{Ordered factor. q34c. My immediate supervisor encourages me, with Likert scale likelihood levels.}
#'   \item{\code{q34d}}{Ordered factor. q34d. I receive the respect I deserve from my colleagues at work, with Likert scale likelihood levels.}
#'   \item{\code{q34e}}{Ordered factor. q34e. I am clear what my duties and responsibilities are, with Likert scale likelihood levels.}
#'   \item{\code{q34f}}{Ordered factor. q34f. Relationships at work are strained, with Likert scale likelihood levels.}
#'   \item{\code{q34g}}{Ordered factor. q34g. Staff are consulted about change at work, with Likert scale likelihood levels.}
#'   \item{\code{q34h}}{Ordered factor. q34h. I am expected to do too many different tasks in too little time, with Likert scale likelihood levels.}
#'   \item{\code{q35a}}{Ordered factor. q35a. I am satisfied with the policies/practices in place to help me manage my health and wellbeing, with Likert scale agreement levels.}
#'   \item{\code{q35b}}{Ordered factor. q35b. My agency does a good job of communicating what it can offer me in terms of health and wellbeing, with Likert scale agreement levels.}
#'   \item{\code{q35c}}{Ordered factor. q35c. My agency does a good job of promoting health and wellbeing, with Likert scale agreement levels.}
#'   \item{\code{q35d}}{Ordered factor. q35d. I think my agency cares about my health and wellbeing, with Likert scale agreement levels.}
#'   \item{\code{q35e}}{Ordered factor. q35e. I believe my immediate supervisor cares about my health and wellbeing, with Likert scale agreement levels.}
#'   \item{\code{q35f}}{Ordered factor. q35f. If I felt it was needed, I would feel comfortable discussing my mental health and wellbeing with my supervisor, with Likert scale agreement levels.}
#'   \item{\code{q36}}{Character. q36. In general, would you say that your health is:.}
#'   \item{\code{q37}}{Character. q37. To what extent is your work emotionally demanding?.}
#'   \item{\code{q38}}{Ordered factor. q38. How often do you find your work stressful?, with Likert scale likelihood levels.}
#'   \item{\code{q39}}{Ordered factor. q39. I feel burned out by my work., with Likert scale agreement levels.}
#'   \item{\code{q40}}{Ordered factor. q40. Which of the following statements best reflects your current thoughts about working in your current position?, with specific ordered levels.}
#'   \item{\code{q41}}{Ordered factor. q41. What best describes your plans involved with leaving your current position?, with specific ordered levels.}
#'   \item{\code{q42}}{Factor. q42. What is the primary reason behind your desire to leave your current position?, non-ordered.}
#'   \item{\code{q43a}}{Ordered factor. q43a. I believe that one of my responsibilities is to continually look for new ways to improve the way we work, with Likert scale agreement levels.}
#'   \item{\code{q43b}}{Ordered factor. q43b. My immediate supervisor encourages me to come up with new or better ways of doing things, with Likert scale agreement levels.}
#'   \item{\code{q43c}}{Ordered factor. q43c. People are recognised for coming up with new and innovative ways of working, with Likert scale agreement levels.}
#'   \item{\code{q43d}}{Ordered factor. q43d. My agency inspires me to come up with new or better ways of doing things, with Likert scale agreement levels.}
#'   \item{\code{q43e}}{Ordered factor. q43e. My agency recognises and supports the notion that failure is a part of innovation, with Likert scale agreement levels.}
#'   \item{\code{q44}}{Ordered factor. q44. During the last 12 months and in the course of your employment, have you experienced discrimination on the basis of your background or a personal characteristic?, with levels: "Yes", "No".}
#'   \item{\code{q45}}{Ordered factor. q45. Did this discrimination occur in your current agency?, with levels: "Yes", "No".}
#'   \item{\code{q46_1}}{Character. q46_1. Gender.}
#'   \item{\code{q46_2}}{Character. q46_2. Race.}
#'   \item{\code{q46_3}}{Character. q46_3. Disability (e.g. loss of hearing or sight, incomplete use of limbs, or mental health issues).}
#'   \item{\code{q46_4}}{Character. q46_4. Caring responsibilities.}
#'   \item{\code{q46_5}}{Character. q46_5. Age.}
#'   \item{\code{q46_6}}{Character. q46_6. LGBTIQA+.}
#'   \item{\code{q46_7}}{Character. q46_7. Identification as an Australian Aboriginal and/or Torres Strait Islander person.}
#'   \item{\code{q46_8}}{Character. q46_8. Religion.}
#'   \item{\code{q46_9}}{Character. q46_9. Other.}
#'   \item{\code{q47}}{Ordered factor. q47. During the last 12 months, have you been subjected to harassment or bullying in your current workplace?, with levels: "Yes", "No", "Not Sure".}
#'   \item{\code{q48_1}}{Character. q48_1. Physical behaviour.}
#'   \item{\code{q48_2}}{Character. q48_2. Sexual harassment.}
#'   \item{\code{q48_3}}{Character. q48_3. Cyberbullying (e.g. harassment via IT, or the spreading of gossip/materials intended to defame or humiliate).}
#'   \item{\code{q48_4}}{Character. q48_4. Verbal abuse (e.g. offensive language, derogatory remarks, shouting or screaming).}
#'   \item{\code{q48_5}}{Character. q48_5. 'Initiations' or pranks.}
#'   \item{\code{q48_6}}{Character. q48_6. Interference with your personal property or work equipment.}
#'   \item{\code{q48_7}}{Character. q48_7. Interference with work tasks (e.g. withholding needed information, undermining or sabotage).}
#'   \item{\code{q48_8}}{Character. q48_8. Inappropriate and unfair application of work policies or rules (e.g. performance management, access to leave, access to learning and development).}
#'   \item{\code{q48_9}}{Character. q48_9. Deliberate exclusion from work-related activities.}
#'   \item{\code{q48_10}}{Character. q48_10. Other.}
#'   \item{\code{q49_1}}{Character. q49_1. Your current supervisor.}
#'   \item{\code{q49_2}}{Character. q49_2. A previous supervisor.}
#'   \item{\code{q49_3}}{Character. q49_3. Someone more senior (other than your supervisor).}
#'   \item{\code{q49_4}}{Character. q49_4. Co-worker.}
#'   \item{\code{q49_5}}{Character. q49_5. Contractor.}
#'   \item{\code{q49_6}}{Character. q49_6. Someone more junior than you.}
#'   \item{\code{q49_7}}{Character. q49_7. Client, customer or stakeholder.}
#'   \item{\code{q49_8}}{Character. q49_8. Consultant / service provider.}
#'   \item{\code{q49_9}}{Character. q49_9. Representative of another APS agency.}
#'   \item{\code{q49_10}}{Character. q49_10. Minister or ministerial adviser.}
#'   \item{\code{q49_11}}{Character. q49_11. Unknown.}
#'   \item{\code{q50}}{Factor. q50. Did you report the harassment or bullying?, non-ordered.}
#'   \item{\code{q51_1}}{Character. q51_1. I did not want to upset relationships in the workplace.}
#'   \item{\code{q51_2}}{Character. q51_2. I did not have enough evidence.}
#'   \item{\code{q51_3}}{Character. q51_3. It could affect my career.}
#'   \item{\code{q51_4}}{Character. q51_4. I did not think action would be taken.}
#'   \item{\code{q51_5}}{Character. q51_5. The matter was resolved informally.}
#'   \item{\code{q51_6}}{Character. q51_6. I did not think the harassment or bullying was serious enough.}
#'   \item{\code{q51_7}}{Character. q51_7. Managers accepted the behaviour.}
#'   \item{\code{q51_8}}{Character. q51_8. I did not think it was worth the hassle of going through the reporting process.}
#'   \item{\code{q51_9}}{Character. q51_9. I feared possible retaliation or reprisals.}
#'   \item{\code{q51_10}}{Character. q51_10. I did not know how to report it.}
#'   \item{\code{q51_11}}{Character. q51_11. Other.}
#'   \item{\code{q52}}{Ordered factor. q52. Excluding behaviour reported to you as part of your duties, in the last 12 months have you witnessed another APS employee in your agency engaging in behaviour that you consider may be serious enough to be viewed as corruption?, with levels: "Yes", "No", "Not Sure", "Would prefer not to answer".}
#'   \item{\code{q53_1}}{Character. q53_1. Bribery, domestic and foreign-obtaining, offering or soliciting secret commissions, kickbacks or gratuities.}
#'   \item{\code{q53_2}}{Character. q53_2. Fraud, forgery or embezzlement.}
#'   \item{\code{q53_3}}{Character. q53_3. Theft or misappropriation of official assets.}
#'   \item{\code{q53_4}}{Character. q53_4. Nepotism - preferential treatment of family members, such as appointing them to positions without proper regard to merit.}
#'   \item{\code{q53_5}}{Character. q53_5. Cronyism - preferential treatment of friends, such as appointing them to positions without proper regard to merit.}
#'   \item{\code{q53_6}}{Character. q53_6. Acting (or failing to act) in the presence of an undisclosed conflict of interest.}
#'   \item{\code{q53_7}}{Character. q53_7. Unlawful disclosure of government information.}
#'   \item{\code{q53_8}}{Character. q53_8. Blackmail.}
#'   \item{\code{q53_9}}{Character. q53_9. Perverting the course of justice.}
#'   \item{\code{q53_10}}{Character. q53_10. Colluding, conspiring with, or harbouring criminals.}
#'   \item{\code{q53_11}}{Character. q53_11. Insider trading.}
#'   \item{\code{q53_12}}{Character. q53_12. Green-lighting.}
#'   \item{\code{q53_13}}{Character. q53_13. Other.}
#'   \item{\code{q54}}{Factor. q54. Did you report the potentially corrupt behaviour?, non-ordered.}
#'   \item{\code{q55_1}}{Character. q55_1. I did not want to upset relationships in the workplace.}
#'   \item{\code{q55_2}}{Character. q55_2. I did not have enough evidence.}
#'   \item{\code{q55_3}}{Character. q55_3. It could affect my career.}
#'   \item{\code{q55_4}}{Character. q55_4. I was concerned about adverse consequences beyond the effect on my career.}
#'   \item{\code{q55_5}}{Character. q55_5. I did not think action would be taken.}
#'   \item{\code{q55_6}}{Character. q55_6. I did not think the corruption was serious enough.}
#'   \item{\code{q55_7}}{Character. q55_7. Managers accepted the behaviour.}
#'   \item{\code{q55_8}}{Character. q55_8. I did not think it was worth the hassle of going through the report process.}
#'   \item{\code{q55_9}}{Character. q55_9. I feared possible retaliation or reprisals.}
#'   \item{\code{q55_10}}{Character. q55_10. I did not know how to report it.}
#'   \item{\code{q55_11}}{Character. q55_11. Other.}
#' }
#' @source <https://data.gov.au/data/dataset/2024-aps-employee-census/resource/55b6e7e7-25be-4012-af43-edaaffbd2ace>
"aps_employee_census_2024_individual"
