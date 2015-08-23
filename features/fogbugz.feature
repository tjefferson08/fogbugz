Feature: log on

  Scenario: log on
    Given I start an action chain
    Given the mock is set for "logon"
    And I press "M-x"
    And I type "fogbugz-logon"
    And I execute the action chain
    And I inspect the variable "fogbugz-token"
    Then I should see pattern "fogbugz-token's value is \"123\""

Feature: list filters

  Scenario: open fogbugz filters list
    Given I am logged on
    Given the mock is set for "listFilters"
    And I start an action chain
    And I press "M-x"
    And I type "fogbugz-list-filters"
    And I execute the action chain
    Then I should be in buffer "*fogbugz-filters-list*"
    Then I should see "My Cases"
    Then I should see "Cases I should have closed months ago"
    Then I should see "* Customer Service Top 10"
    Then the cursor should be before "My Cases"

Feature: select filter, show search results

  Scenario: select a filter from the filters list
    Given I am logged on
    Given I am in the filters list buffer
    Given the mock is set for "setCurrentFilter"
    Then I should be in buffer "*fogbugz-filters-list*"
    Then I should see "* Customer Service Top 10"
    # Given the mock is set for "search"
    # And I press "<RET>"
    # Then I should be in buffer "*fogbugz-search-results*"
    # Then I should see "1234"

Feature: Open case under point
  Scenario: from search results, open a case in the same window
    Given I am logged on
    Given I am in the search results buffer
    Given the mock is set for "case"
    Then I should be in buffer "*fogbugz-search-results*"
    Then I should see "1234"
    Given I go to word "1234"
    Then I start an action chain
    And I press "M-x"
    And I type "fogbugz-open-case-under-point"
    And I execute the action chain
    Then I should be in buffer "*fogbugz-case-1234*"
    Then I should see "1234"
    Then I should see "Title: Fire the intern"
    Then I should see "Opened by Bob"
    Then I should see "Assigned to Bill by Bob"
