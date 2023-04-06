cities <- unique(t20i_match_info$city)
hosts <- t20i_match_info %>% mutate(country = case_when(
  ((city=="Adelaide") | (city=="Melbourne") | 
     (city=="Brisbane") | (city=="Perth") | (city=="Hobart") |
     (city=="Sydney") | (city=="Canberra") | city=="Carrara") ~ 'Australia',
  ((city=="Southampton") | (city=="Bristol") | (city=="London") | 
     (city=="Manchester") | (city=="Nottingham") | (city=="Birmingham") |
     (city=="Cardiff") | (city=="Chester-le-Street") | (city=="East London") |
    (city=="Taunton") | (city=="Leeds")) ~ 'England',
  ((city=="Johannesburg") | (city=="Cape Town") | (city=="Durban") | 
     (city=="Port Elizabeth") | (city=="Centurion") | (city=="Bloemfontein") |
     (city=="Potchefstroom") | city =="Kimberley" | city == "Paarl") ~ 'South Africa',
  ((city=="Barbados") | (city=="Trinidad") | (city=="St Kitts") | 
     (city=="St Lucia") | (city=="Guyana") | (city=="Antigua") |
     (city=="St Vincent") | (city=="Dominica") | (city=="Jamaica") | 
     city=="Kingston" | city=="Basseterre" | city == "Bridgetown" |
        city== "Gros Islet" | city=="Lauderhill" |city == "Providence" |
     city=="Roseau" | city =="St George's" | city == "Tarouba") ~ 'West Indies',
  ((city=="Mumbai") | city == "Cuttack" | city == "Dehra Dun" |
     (city=="Kolkata") | (city=="Chennai") | (city=="Pune") |
      (city=="Delhi") | (city=="Nagpur") | (city=="Lucknow") |
     (city=="Hyderabad") | (city=="Jaipur") | (city=="Ahmedabad") |
     (city=="Bangalore")| (city=="Bengaluru")|city=="Chandigarh" |
     city == "Dehradun" | city=="Dharmasala" | city=="Dharamsala" | city=="Guwahati" |
     city == "Kanpur" | city == "Ranchi" | city=="Thiruvananthapuram" |
     city == "Visakhapatnam" | city == "Rajkot" | city  == "Indore") ~ 'India',
  ((city=="Auckland") | (city=="Christchurch") | (city=="Wellington") | 
     (city=="Mount Maunganui") | city=="Hamilton" | city=="Napier" |
     city == "Nelson" | city=="Dunedin") ~ 'new Zealand',
  city=="Abu Dhabi" | city=="Dubai" | city=="Nairobi" | city == "Sharjah"~ 'Other',
  (city=="Mirpur") | (city=="Chattogram") | (city=="Chittagong") | 
          (city=="Dhaka") | (city=="Khulna") | (city=="Sylhet") ~ 'Bangladesh',
   (city=="Karachi") | (city=="Lahore") ~ 'Pakistan',
   city=="Harare" | city=="Bulawayo"  ~ 'Zimbabwe'))
