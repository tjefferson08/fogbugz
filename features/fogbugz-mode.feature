Feature: log on
  In order to do log on
  As a user

  Scenario: log on
    Given I start an action chain
    And the mock is set for "logon"
    And I press "M-x"
    And I type "fogbugz-logon"
    And I execute the action chain
    And I inspect the variable "fogbugz-token"
    Then I should see message "123"

Feature: list filters

  Scenario: open fogbugz filters list
    Given I am logged on
    And I start an action chain
    And the mock is set for "listFilters"
    And I press "M-x"
    And I type "fogbugz-list-filters"
    And I execute the action chain
    Then I should be in buffer "*fogbugz-filters-list*"
    Then I should see "My Cases"
    Then I should see "Cases I should have closed months ago"
    Then I should see "* Customer Service Top 10"
    Then the cursor should be before "My Cases"
