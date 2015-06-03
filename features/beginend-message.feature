Feature: When in message mode, begin and end are changed
  In order to navigate a message buffer easily
  As a user
  I want that M-< go to the beginning of message body and M-> go to the line before signature

  Background:
    Given I setup a message

  Scenario: Press M-< or M-> once
    Given I press "M-<"
    Then I should be before "Hello,"
    Given I press "M->"
    Then I should be after "Bye,"

  Scenario: Press M-< or M-> twice
    Given I press "M-<"
    Then I should be before "Hello,"
    Given I press "M-<"
    Then I should be before "From:"
    And I should be at beginning of buffer

    Given I press "M->"
    Then I should be after "Bye,"
    Given I press "M->"
    Then I should be at end of buffer
