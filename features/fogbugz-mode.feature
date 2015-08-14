Feature: log on
  In order to do log on
  As a user

  Scenario: log on
    Given I start an action chain
    And the mock is set for "logon"
    And I press "M-x"
    And I type "fogbugz-logon"
    And I execute the action chain
    And I inspect the variable fogbugz-token
    Then I should see message "123"
