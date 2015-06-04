Feature: When in message mode, begin and end are changed
  In order to navigate a message buffer easily
  As a user
  I want that M-< go to the beginning of message body and M-> go to the line before signature

  Background:
    Given I setup a message

  Scenario: Press M-< once
    Given I activate beginend-message-mode

    # avoid being at the right position already
    Given I press "M->"

    When I press "M-<"
    Then I should be before "Hello,"

  Scenario: Press M-> once
    Given I activate beginend-message-mode

    # avoid being at the right position already
    Given I press "M-<"

    When I press "M->"
    Then I should be after "Bye,"

  Scenario: Press M-< twice
    Given I activate beginend-message-mode

    # avoid being at the right position already
    Given I press "M->"

    Given I press "M-<"
    And I should be before "Hello,"

    When I press "M-<"
    Then I should be at beginning of buffer

  Scenario: Press M-> twice
    Given I activate beginend-message-mode

    # avoid being at the right position already
    Given I press "M-<"

    Given I press "M->"
    And I should be after "Bye,"

    When I press "M->"
    Then I should be at end of buffer
