#Data request from Sheel
#4/6/15: "I'm doing some analysis on HQ and I'm trying to find all forms 
#submitted for a given XMLNS, irrespective of which project."
#RD to create a table with one row per domain with the number of forms 
#submitted with each xmlns that Sheel is interested in.

#First import form_table and formdef table from DB
formdef <- filter(formdef, !is.na(xmlns))
form_table <- filter(form_table, !is.na(formdef_id))

#Also import domain table
domains <- get_domain_table(db)
names(domains)[names(domains) == "id"] = "domain_id"
domains <- select(domains, name, domain_id)

#Character vectors of xmlns of interest
mvp_xmlns <- c("http://openrosa.org/formdesigner/882FC273-E436-4BA1-B8CC-9CA526FFF8C2", 
               "http://openrosa.org/formdesigner/185A7E63-0ECD-4D9A-8357-6FD770B6F065", 
               "http://openrosa.org/formdesigner/E6511C2B-DFC8-4DEA-8200-CC2F2CED00DA", 
               "http://openrosa.org/formdesigner/B9CEFDCD-8068-425F-BA67-7DC897030A5A", 
               "http://openrosa.org/formdesigner/8C652174-BDAE-4300-AEF3-B2F30B20069D", 
               "http://openrosa.org/formdesigner/266AD1A0-9EAE-483E-B4B2-4E85D6CA8D4B")

remind_xmlns <- c("http://openrosa.org/formdesigner/E4AFA1AA-1ADA-4E39-90C9-493A7781BBE0", 
               "http://openrosa.org/formdesigner/4A49479F-BEBD-498D-A08B-4F0EAAD4DDBB", 
               "http://openrosa.org/formdesigner/FDD31334-4885-4C31-AEA4-59547CCC5C9E", 
               "http://openrosa.org/formdesigner/93BC80C0-E9EC-4A0E-BDE0-ACE69CB30B88")

farmer_xmlns <- c("http://openrosa.org/formdesigner/0612C3D0-85FC-4504-A1E7-19463AB45F51", 
                  "http://openrosa.org/formdesigner/ABA67EE9-F6B9-4FEB-B048-7327651D9245")

sneha_xmlns <- c("http://openrosa.org/formdesigner/1C98084A-A01F-44E3-A0CC-0448DB251C68", 
                  "http://openrosa.org/formdesigner/4EED9EC5-5C14-444F-A435-CFE03E7008F2", 
                  "http://openrosa.org/formdesigner/3cf38d003524fade0b3f5d54bb79b251f1db81f3", 
                  "http://openrosa.org/formdesigner/3a3efa1f413bc0ad029598c690788efde17566ce")

#Variables of interest in fomdef table are (1) xmlns and (2) id
#The id variable from the formdef table is the formdef_id variable in 
#the form_table.
#Flag form ids for each xmlns of interest

mvp1 <- formdef$id[formdef$xmlns == mvp_xmlns[1]]
mvp2 <- formdef$id[formdef$xmlns == mvp_xmlns[2]]
mvp3 <- formdef$id[formdef$xmlns == mvp_xmlns[3]]
mvp4 <- formdef$id[formdef$xmlns == mvp_xmlns[4]]
mvp5 <- formdef$id[formdef$xmlns == mvp_xmlns[5]]
mvp6 <- formdef$id[formdef$xmlns == mvp_xmlns[6]]

remind1 <- formdef$id[formdef$xmlns == remind_xmlns[1]]
remind2 <- formdef$id[formdef$xmlns == remind_xmlns[2]]
remind3 <- formdef$id[formdef$xmlns == remind_xmlns[3]]
remind4 <- formdef$id[formdef$xmlns == remind_xmlns[4]]

farmer1 <- formdef$id[formdef$xmlns == farmer_xmlns[1]]
farmer2 <- formdef$id[formdef$xmlns == farmer_xmlns[2]]

sneha1 <- formdef$id[formdef$xmlns == sneha_xmlns[1]]
sneha2 <- formdef$id[formdef$xmlns == sneha_xmlns[2]]
sneha3 <- formdef$id[formdef$xmlns == sneha_xmlns[3]]
sneha4 <- formdef$id[formdef$xmlns == sneha_xmlns[4]]

#Count number of forms submissions for each xmlns by domain
forms_xmlns <- form_table[form_table$formdef_id %in% sneha4,]
domain_xml <- forms_xmlns %>% group_by(domain_id) %>% 
  summarise(nforms = length(unique(form_id)))
domain_xml <- merge(domain_xml, domains, by = "domain_id", all.x = T)
write.csv(domain_xml, file = "domain_xml.csv")

