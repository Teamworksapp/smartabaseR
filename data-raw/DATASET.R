## code to prepare `DATASET` dataset goes here

user_json <-
  '
{
  "results": [
    {
      "search": {
        "username": "jamie.anderson"
      },
      "results": [
        {
          "userId": 12000,
          "firstName": "Jamie",
          "lastName": "Anderson",
          "dob": "01/01/2000",
          "middleName": "",
          "emailAddress": "jamie.anderson@teamworks.com",
          "knownAs": "",
          "organisationalId": -1,
          "username": "jamie.anderson",
          "phoneNumbers": [
            {
              "type": "HOME",
              "countryCode": "61",
              "prefix": "07",
              "number": "31237124"
            },
            {
              "type": "HOME",
              "countryCode": "61",
              "prefix": "07",
              "number": "12345678"
            },
            {
              "type": "EMERGENCY",
              "countryCode": "61",
              "prefix": "07",
              "number": "99999999"
            }
          ],
          "addresses": [
            {
              "type": "PRIMARY",
              "address": "North Tower, Level 1/339 Coronation Drive",
              "suburb": "Milton",
              "city": "QLD",
              "country": "Australia",
              "postcode": "4064"
            },
            {
              "type": "ALTERNATIVE",
              "address": "76 Neon Street",
              "suburb": "Sumner",
              "city": "QLD",
              "country": "Australia",
              "postcode": "4074"
            },
            {
              "type": "ALTERNATIVE",
              "address": "109 Kirribilli Ave",
              "suburb": "Kirribilli",
              "city": "NSW",
              "country": "Australia",
              "postcode": "2061"
            }
          ],
          "sex": "MALE",
          "groupsAndRoles": {
            "coachGroups": [
              {
                "id": 3000,
                "name": "Demo Group"
              },
              {
                "id": 4000,
                "name": "All Users"
              }
            ],
            "athleteGroups": [
              {
                "id": 3000,
                "name": "Demo Group"
              },
              {
                "id": 4000,
                "name": "All Users"
              }
            ],
            "role": [
              {
                "id": 700,
                "name": "Demo Role"
              }
            ]
          },
          "uuid": "123"
        }
      ]
    }
  ]
}
'


athlete_group_list <- list(
  tibble::tibble(
    athlete_group_id = c(3000, 4000),
    athlete_group_name = c("Demo Group", "All Users")
  )
)

coach_group_list <- list(
  tibble::tibble(
    coach_group_id = c(3000, 4000),
    coach_group_name = c("Demo Group", "All Users")
  )
)

role_list <- list(
  tibble::tibble(
    role_id = 700,
    role_name = "Demo Role"
  )
)

phone_list <- list(
  tibble::tibble(
    phone_type_count = c(1, 2, 1),
    phone_type = c("phone_home", "phone_home", "phone_emergency"),
    phone_number = c("610731237124", "610712345678", "610799999999")
  )
)

address_list <- list(
  tibble::tibble(
    address_type_count = c(1, 1, 2),
    address_type = c(
      "address_primary",
      "address_alternative",
      "address_alternative"
    ),
    address = c(
      "North Tower, Level 1/339 Coronation Drive Milton QLD Australia 4064",
      "76 Neon Street Sumner QLD Australia 4074",
      "109 Kirribilli Ave Kirribilli NSW Australia 2061"
    )
  )
)

user_data_joined <- tibble::tibble(
  document.id = 1,
  export_object = "results",
  record_number = 1,
  results_number = 1,
  userId = 12000,
  firstName = "Jamie",
  lastName = "Anderson",
  dob = "01/01/2000",
  middleName = "",
  emailAddress = "jamie.anderson@teamworks.com",
  knownAs = "",
  organisationalId = -1,
  username = "jamie.anderson",
  sex = "MALE",
  uuid = "123",
  athlete_group = athlete_group_list,
  coach_group = coach_group_list,
  role = role_list,
  phone = phone_list,
  address = address_list
)

user_data_clean <- tibble::tibble(
  user_id = 12000,
  first_name = "Jamie",
  last_name = "Anderson",
  username = "jamie.anderson",
  uuid = "123",
  dob = "01/01/2000",
  middle_name = "",
  known_as = "",
  email = "jamie.anderson@teamworks.com",
  sex = "MALE"
)


usethis::use_data(user_json, user_data_joined, user_data_clean, internal = TRUE, overwrite = TRUE)
