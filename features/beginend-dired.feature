Feature: When in dired mode, begin and end are changed
  In order to navigate a dired buffer easily
  As a user
  I want that M-< and M-> go to first and last file

  Background:
    Given I setup dired

  Scenario outline: Press M-< once
    Given I <activate omit> dired-omit-mode
    And I <activate hide> dired-hide-details-mode

    # avoid being at the right position already
    And I press "M->"

    When I press "M-<"
    Then I should be before "file1"

    Examples:
    | activate omit | activate hide |
    | activate      | activate      |
    | activate      | deactivate    |
    | deactivate    | activate      |
    | deactivate    | deactivate    |

  Scenario outline: Press M-> once
    Given I <activate omit> dired-omit-mode
    And I <activate hide> dired-hide-details-mode

    # avoid being at the right position already
    And I press "M-<"

    When I press "M->"
    Then I should be after "file2"

    Examples:
    | activate omit | activate hide |
    | activate      | activate      |
    | activate      | deactivate    |
    | deactivate    | activate      |
    | deactivate    | deactivate    |

  Scenario: Press M-< twice
    # avoid being at the right position already
    Given I press "M->"

    Given I press "M-<"
    And I should be before "file1"

    When I press "M-<"
    Then I should be at beginning of buffer

  Scenario: Press M-> twice
    # avoid being at the right position already
    Given I press "M-<"

    Given I press "M->"
    And I should be after "file2"

    When I press "M->"
    Then I should be at end of buffer
