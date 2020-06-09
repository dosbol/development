package discourje.core.graph;

import java.util.*;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class Edge<State, Action, Test extends Predicate<Object> & Supplier<Object>> {

    public final Vertex<State, Action, Test> source;
    public final Vertex<State, Action, Test> target;
    public final Label<Action, Test> label;

    public Edge(Vertex<State, Action, Test> source, Vertex<State, Action, Test> target, Label<Action, Test> label) {
        this.source = source;
        this.target = target;
        this.label = label;
    }

    //
    // java.lang.Object
    //

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Edge<?, ?, ?> edge = (Edge<?, ?, ?>) o;
        return Objects.equals(source, edge.source) &&
                Objects.equals(target, edge.target) &&
                Objects.equals(label, edge.label);
    }

    @Override
    public int hashCode() {
        return Objects.hash(source, target, label);
    }

    //
    // Edge.Collection
    //

    public static class Collection<State, Action, Test extends Predicate<Object> & Supplier<Object>> extends AbstractCollection<Edge<State, Action, Test>> {

        private Set<Edge<State, Action, Test>> edges = new LinkedHashSet<>();

        private Map<Action, Map<Test, Set<Vertex<State, Action, Test>>>> targets = new LinkedHashMap<>();

        //
        // java.util.AbstractCollection
        //

        @Override
        public boolean add(Edge<State, Action, Test> e) {
            var l = e.label;
            var map = targets.computeIfAbsent(l.action, a -> new LinkedHashMap<>());
            var set = map.computeIfAbsent(l.test, t -> new LinkedHashSet<>());
            set.add(e.target);
            return edges.add(e);
        }

        @Override
        public Iterator<Edge<State, Action, Test>> iterator() {
            return edges.iterator();
        }

        @Override
        public int size() {
            var size = 0;
            for (Map<Test, Set<Vertex<State, Action, Test>>> m : targets.values()) {
                for (Set<Vertex<State, Action, Test>> s : m.values()) {
                    size += s.size();
                }
            }
            return size;
        }

        //
        // Collection
        //

        public Set<Edge.Label<Action, Test>> getLabels() {
            var labels = new LinkedHashSet<Edge.Label<Action, Test>>();
            for (Edge<State, Action, Test> e : edges) {
                labels.add(e.label);
            }
            return labels;
        }

        public Set<Vertex<State, Action, Test>> getTargets() {
            var targets = new LinkedHashSet<Vertex<State, Action, Test>>();
            for (Map<Test, Set<Vertex<State, Action, Test>>> m : this.targets.values()) {
                for (Set<Vertex<State, Action, Test>> s : m.values()) {
                    targets.addAll(s);
                }
            }
            return targets;
        }

        public Set<Vertex<State, Action, Test>> getTargets(Edge.Label<Action, Test> l) {
            var targets = new LinkedHashSet<Vertex<State, Action, Test>>();

            var m = this.targets.get(l.action);
            if (m != null) {
                for (Map.Entry<Test, Set<Vertex<State, Action, Test>>> e : m.entrySet()) {
                    if (l.test.get().equals(e.getKey().get())) {
                        targets.addAll(e.getValue());
                    }
                }
            }

            return targets;
        }

        public Set<Vertex<State, Action, Test>> getTargets(Action a, Object o) {
            var targets = new LinkedHashSet<Vertex<State, Action, Test>>();

            var m = this.targets.get(a);
            if (m != null) {
                for (Map.Entry<Test, Set<Vertex<State, Action, Test>>> e : m.entrySet()) {
                    if (e.getKey().test(o)) {
                        targets.addAll(e.getValue());
                    }
                }
            }

            return targets;
        }
    }

    //
    // Edge.Label
    //

    public static class Label<Action, Test extends Predicate<?>> {

        public final String name;
        public final Action action;
        public final Test test;

        public Label(String name, Action action, Test test) {
            this.name = name;
            this.action = action;
            this.test = test;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Label<?, ?> label = (Label<?, ?>) o;
            return Objects.equals(name, label.name);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name);
        }

        @Override
        public String toString() {
            return name;
        }
    }
}
