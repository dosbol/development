package discourje.core.validation.operators;

import discourje.core.validation.DMState;
import discourje.core.validation.DiscourjeModel;
import java.util.Objects;

class True implements CtlOperator {
    public static final True TRUE = new True();
    private final int hash;

    private True() {
        hash = Objects.hash(this);
    }

    @Override
    public void label(DiscourjeModel<?> model) {
        if (!model.isLabelledBy(this)) {
            for (DMState<?> state : model.getStates()) {
                state.addLabel(this);
            }
            model.setLabelledBy(this);
        }
    }

    public String toString() {
        return "true";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        return o != null && getClass() == o.getClass();
    }

    @Override
    public int hashCode() {
        return hash;
    }
}
