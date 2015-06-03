Feature: When in dired mode, begin and end are changed
  In order to navigate a dired buffer easily
  As a user
  I want that M-< and M-> go to first and last file

  Background:
    Given I setup dired

  Scenario: Press M-< or M-> once
    Given I deactivate dired-omit-mode
    And I deactivate dired-hide-details-mode

    # avoid being at the right position already
    And I press "M->"

    And I press "M-<"
    Then I should be before "file1"
    Given I press "M->"
    Then I should be after "file2"

    Given I deactivate dired-omit-mode
    And I activate dired-hide-details-mode
    And I press "M-<"
    Then I should be before "file1"
    Given I press "M->"
    Then I should be after "file2"

    Given I deactivate dired-omit-mode
    And I activate dired-hide-details-mode
    And I press "M-<"
    Then I should be before "file1"
    Given I press "M->"
    Then I should be after "file2"

    Given I activate dired-omit-mode
    And I activate dired-hide-details-mode
    And I press "M-<"
    Then I should be before "file1"
    Given I press "M->"
    Then I should be after "file2"

  Scenario: Press M-< or M-> twice
    Given I deactivate dired-omit-mode
    And I deactivate dired-hide-details-mode
    And I press "M-<"
    Then I should be before "file1"
    Given I press "M-<"
    And I should be at beginning of buffer
    Given I press "M->"
    Then I should be after "file2"
    Given I press "M->"
    And I should be at end of buffer
