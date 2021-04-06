package discourje.core.validation.rules;

import discourje.core.validation.Rule;
import discourje.core.validation.formulas.CtlFormula;
import static discourje.core.validation.formulas.CtlFormulas.AF;
import static discourje.core.validation.formulas.CtlFormulas.AG;
import static discourje.core.validation.formulas.CtlFormulas.close;
import static discourje.core.validation.formulas.CtlFormulas.implies;
import static discourje.core.validation.formulas.CtlFormulas.send;

public class UsedChannelsMustBeClosed extends Rule {

    @Override
    public String createErrorDescription(String r1, String r2) {
        return String.format("A message is sent from %s to %s, but the channel is not closed afterwards.", r1, r2);
    }

    @Override
    public CtlFormula createCtlFormula(String r1, String r2) {
        return AG(implies(send(r1, r2), AF(close(r1, r2))));
    }
}
