#include <iostream>
#include <stdexcept>
#include <tuple>
#include <vector>

template <typename Key, typename Value, typename Hash = std::hash<Key>,
          typename Equal = std::equal_to<Key>,
          typename Alloc = std::allocator<std::pair<const Key, Value>>>
class UnorderedMap {

    using Self = UnorderedMap;
    using AllocTraits = std::allocator_traits<Alloc>;

    template <typename T, typename ListAlloc>
    class List {
        friend class UnorderedMap;

      private:
        template <typename RefType>
        class ListIterator;

      public:
        using Self = List;

        using size_type = size_t;
        using difference_type = int64_t;

        using value_type = T;
        using reference = T&;
        using const_reference = const T&;
        using iterator = ListIterator<reference>;
        using const_iterator = ListIterator<const_reference>;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        using allocator_type = typename ListAlloc::value_type;

        using ListAllocTraits = typename std::allocator_traits<ListAlloc>;

      private:
        struct BaseNode {
            BaseNode *next, *prev;
            BaseNode()
                : next(this), prev(this) {}
            BaseNode(const BaseNode& other)
                : next(other.next), prev(other.prev) {}
            BaseNode(BaseNode&& other)
                : next(other.next), prev(other.prev) {
                other.next = &other;
                other.prev = &other;
            }

            BaseNode& operator=(BaseNode&& other) {
                next = other.next;
                prev = other.prev;
                other.next = other.prev = &other;
                return *this;
            }

            void swap(BaseNode& other) {
                if (this == &other) {
                    return;
                }
                if (this == next && &other == other.next) {
                    return;
                }
                if (this == next) {
                    other.next->prev = this;
                    other.prev->next = this;
                    next = other.next;
                    prev = other.prev;
                    other.next = other.prev = &other;
                    return;
                }
                if (&other == other.next) {
                    next->prev = &other;
                    prev->next = &other;
                    other.next = next;
                    other.prev = prev;
                    next = prev = this;
                    return;
                }
                next->prev = &other;
                prev->next = &other;
                other.next->prev = this;
                other.prev->next = this;
                std::swap(next, other.next);
                std::swap(prev, other.prev);
            }
        };

        struct Node : public BaseNode {
            ListAlloc allocator;
            T* data;

            template <typename... Args>
            Node(ListAlloc allocator, Args&&... args)
                : BaseNode(), allocator(allocator), data(ListAllocTraits::allocate(allocator, 1)) {
                ListAllocTraits::construct(allocator, data,
                                           std::forward<Args>(args)...);
            }

            Node(ListAlloc allocator, const Node& other)
                : BaseNode(other), allocator(allocator), data(ListAllocTraits::allocate(allocator, 1)) {
                ListAllocTraits::construct(allocator, data, *other.data);
            }

            Node(ListAlloc allocator, Node&& other)
                : BaseNode(other), allocator(allocator), data(other.data) {
                other.data = nullptr;
            }
            ~Node() {
                if (data != nullptr) {
                    ListAllocTraits::destroy(allocator, data);
                    ListAllocTraits::deallocate(allocator, data, 1);
                }
            }
        };

        template <typename RefType>
        class ListIterator {

            friend class List;

          public:
            using Self = ListIterator;
            using value_type = List::value_type;
            using difference_type = int64_t;
            using reference = RefType;
            using pointer = std::remove_reference_t<RefType>*;
            using iterator_category = std::bidirectional_iterator_tag;

          private:
            BaseNode* node;

          public:
            ListIterator(BaseNode* node)
                : node(node) {}
            ListIterator(const Self& other)
                : node(other.node) {}
            operator const_iterator() const {
                return const_iterator(node);
            }

            RefType operator*() {
                return *static_cast<Node*>(node)->data;
            }

            const std::remove_reference_t<RefType>& operator*() const {
                return *static_cast<Node*>(node)->data;
            }

            std::remove_reference_t<RefType>* operator->() {
                return static_cast<Node*>(node)->data;
            }

            const std::remove_reference_t<RefType>* operator->() const {
                return static_cast<Node*>(node)->data;
            }

            Self& operator=(const Self& other) {
                if (this == &other) {
                    return *this;
                }
                Self tmp = other;
                swap(tmp);
                return *this;
            }

            void swap(Self& other) {
                std::swap(node, other.node);
            }

            Self& operator++() {
                node = node->next;
                return *this;
            }

            Self operator++(int) {
                Self tmp = *this;
                ++*this;
                return tmp;
            }

            Self& operator--() {
                node = node->prev;
                return *this;
            }

            Self operator--(int) {
                Self tmp = *this;
                --*this;
                return tmp;
            }

            template <typename OtherRefType>
            bool operator==(const ListIterator<OtherRefType>& other) const {
                return node == other.node;
            }
        };

        using NodeAlloc = typename ListAllocTraits::template rebind_alloc<Node>;
        using NodeAllocTraits = typename std::allocator_traits<NodeAlloc>;

        ListAlloc allocator;
        NodeAlloc node_allocator;
        BaseNode end_node;
        iterator end_it;
        size_type size_;

      public:
        List(const ListAlloc& alloc = ListAlloc())
            : allocator(alloc), node_allocator(alloc), end_node(), end_it(&end_node), size_(0) {}

        List(const Self& other)
            : allocator(ListAllocTraits::select_on_container_copy_construction(
                  other.get_allocator())),
              node_allocator(allocator),
              end_node(),
              end_it(&end_node),
              size_(0) {
            try {
                for (const_iterator begin_o = other.cbegin(); begin_o != other.end_it;
                     ++begin_o) {
                    push_back(*begin_o);
                }
            } catch (...) {
                while (begin() != end_it) {
                    pop_back();
                }
                throw;
            }
        }

        List(Self&& other)
            : allocator(other.allocator), node_allocator(allocator), end_node(other.end_node), end_it(other.end_it), size_(other.size_) {
            other.end_node.next = other.end_node.prev = &other.end_node;
            end_node.next->prev = end_node.prev->next = &end_node;
            other.end_it = iterator(&other.end_node);
            other.size_ = 0;
        }

        List(size_type n, const T& data = T(), const ListAlloc& alloc = ListAlloc())
            : allocator(alloc), node_allocator(allocator), end_node(), end_it(&end_node), size_(0) {
            try {
                for (size_type i = 0; i < n; ++i) {
                    push_back(data);
                }
            } catch (...) {
                while (begin() != end_it) {
                    pop_back();
                }
                throw;
            }
        }

        List(size_type n, const ListAlloc& alloc)
            : allocator(alloc), node_allocator(allocator), end_node(), end_it(&end_node), size_(0) {
            try {
                for (size_type i = 0; i < n; ++i) {
                    emplace(end_it);
                }
            } catch (...) {
                while (begin() != end_it) {
                    pop_back();
                }
            }
        }

        ~List() {
            while (end_it != begin()) {
                pop_back();
            }
        }

        ListAlloc get_allocator() const {
            return allocator;
        }

        bool operator==(const Self& other) const {
            if (size_ != other.size_) {
                return false;
            }
            for (const_iterator l_begin = cbegin(), r_begin = other.cbegin();
                 l_begin != end_it; ++l_begin, ++r_begin) {
                if (*l_begin != *r_begin) {
                    return false;
                }
            }
            return true;
        }

        Self& operator=(const Self& other) {
            if (this == &other) {
                return *this;
            }
            while (size() != 0) {
                pop_back();
            }
            if constexpr (std::is_base_of_v<
                              std::true_type,
                              typename ListAllocTraits::
                                  propagate_on_container_copy_assignment>) {
                allocator = other.allocator;
                node_allocator = other.node_allocator;
            }
            for (const auto& i : other) {
                push_back(i);
            }

            return *this;
        }

        Self& operator=(Self&& other) {
            if (this == &other) {
                return *this;
            }
            while (size() != 0) {
                pop_back();
            }
            if constexpr (std::is_base_of_v<
                              std::true_type,
                              typename ListAllocTraits::
                                  propagate_on_container_move_assignment>) {
                allocator = std::move(other.allocator);
                node_allocator = std::move(other.node_allocator);
            }
            end_node = std::move(other.end_node);
            end_node.prev->next = &end_node;
            end_node.next->prev = &end_node;
            end_it = iterator(&end_node);
            size_ = std::move(other.size_);
            other.size_ = 0;
            other.end_it = iterator(&other.end_node);
            return *this;
        }

        void swap(Self& other) {
            if (this == &other) {
                return;
            }
            end_node.swap(other.end_node);
            end_it = iterator(&end_node);
            other.end_it = iterator(&other.end_node);
            if constexpr (std::is_base_of_v<std::true_type,
                                            typename ListAllocTraits::
                                                propagate_on_container_swap>) {
                std::swap(allocator, other.allocator);
                std::swap(node_allocator, other.node_allocator);
            }
            std::swap(size_, other.size_);
        }

        size_type size() const {
            return size_;
        }

        void push_back(const T& data) {
            insert(end_it, data);
        }
        void push_back(T&& data) {
            insert(end_it, std::move(data));
        }

        void push_front(const T& data) {
            insert(begin(), data);
        }
        void push_front(T&& data) {
            insert(begin(), std::move(data));
        }

        void pop_back() {
            erase(iterator(end_it.node->prev));
        }

        void pop_front() {
            erase(begin());
        }

        void insert(const_iterator it, const T& data) {
            emplace(it, data);
        }
        void insert(const_iterator it, T&& data) {
            emplace(it, std::move(data));
        }

        void erase(const_iterator it) {
            BaseNode* del_node = it.node;
            del_node->prev->next = del_node->next;
            del_node->next->prev = del_node->prev;
            NodeAllocTraits::destroy(node_allocator, static_cast<Node*>(del_node));
            NodeAllocTraits::deallocate(node_allocator, static_cast<Node*>(del_node),
                                        1);
            --size_;
        }

        template <typename... Args>
        void emplace(const_iterator it, Args&&... args) {
            Node* new_node = NodeAllocTraits::allocate(node_allocator, 1);
            try {
                NodeAllocTraits::construct(node_allocator, new_node, allocator,
                                           std::forward<Args>(args)...);
            } catch (...) {
                NodeAllocTraits::deallocate(node_allocator, new_node, 1);
                throw;
            }

            new_node->next = it.node;
            new_node->prev = it.node->prev;
            new_node->prev->next = new_node;
            new_node->next->prev = new_node;
            ++size_;
        }

        iterator begin() {
            return iterator(end_it.node->next);
        }

        iterator end() {
            return end_it;
        }

        const_iterator cbegin() const {
            return const_iterator(iterator(end_it.node->next));
        }
        const_iterator begin() const {
            return cbegin();
        }

        const_iterator cend() const {
            return end_it;
        }
        const_iterator end() const {
            return cend();
        }

        reverse_iterator rbegin() {
            return reverse_iterator(end_it);
        }

        reverse_iterator rend() {
            return reverse_iterator(begin());
        }

        const_reverse_iterator crbegin() const {
            return const_reverse_iterator(end_it);
        }
        const_reverse_iterator rbegin() const {
            return crbegin();
        }

        const_reverse_iterator crend() const {
            return const_reverse_iterator(cbegin());
        }
        const_reverse_iterator rend() const {
            return crend();
        }
    };

  public:
    using NodeType = std::pair<const Key, Value>;

    using size_type = size_t;
    using difference_type = int64_t;
    using hash_type = size_type;

    using value_type = NodeType;
    using reference = value_type&;
    using const_reference = const value_type&;

    using allocator_type = typename AllocTraits::value_type;

  private:
    using NonConstNodeType = std::pair<Key, Value>;
    struct MapNode {
        Alloc allocator_;
        NodeType* pr;
        hash_type hash_;

        template <typename... Args>
        MapNode(Alloc allocator, Args&&... args)
            : allocator_(allocator), pr(AllocTraits::allocate(allocator, 1)) {
            AllocTraits::construct(allocator_, pr, std::forward<Args>(args)...);
            hash_ = get_hash(pr->first);
        }

        MapNode(Alloc allocator, const Key& key)
            : allocator_(allocator), pr(AllocTraits::allocate(allocator, 1)) {
            AllocTraits::construct(allocator_, pr, key, std::move(Value()));
            hash_ = get_hash(pr->first);
        }

        MapNode(Alloc allocator, Key&& key)
            : allocator_(allocator), pr(AllocTraits::allocate(allocator, 1)) {
            AllocTraits::construct(allocator_, pr, std::move(key),
                                   std::move(Value()));
            hash_ = get_hash(pr->first);
        }

        MapNode(Alloc allocator, const MapNode& other)
            : allocator_(allocator), pr(AllocTraits::allocate(allocator, 1)) {
            AllocTraits::construct(allocator_, pr, *other.pr);
            hash_ = get_hash(pr->first);
        }

        MapNode(Alloc allocator, MapNode&& other)
            : allocator_(allocator), pr(other.pr), hash_(other.hash_) {
            other.pr = nullptr;
        }

        ~MapNode() {
            if (pr != nullptr) {
                AllocTraits::destroy(allocator_, pr);
                AllocTraits::deallocate(allocator_, pr, 1);
            }
        }
    };

    using MapNodeAlloc =
        typename std::allocator_traits<Alloc>::template rebind_alloc<MapNode>;
    using MapNodeAllocTraits = typename std::allocator_traits<MapNodeAlloc>;
    using MapNodeList = List<MapNode, MapNodeAlloc>;

    template <typename NodeRefType, typename ListIterRefType>
    class MapIterator;

  public:
    using iterator = MapIterator<NodeType&, MapNode&>;
    using const_iterator = MapIterator<const NodeType&, const MapNode&>;

  private:
    template <typename NodeRefType, typename ListIterRefType>
    class MapIterator {
        using BaseIter =
            typename MapNodeList::template ListIterator<ListIterRefType>;

      public:
        using Self = MapIterator;
        using value_type = UnorderedMap::value_type;
        using difference_type = int64_t;
        using reference = NodeRefType;
        using pointer = std::remove_reference_t<NodeRefType>*;
        using iterator_category = std::forward_iterator_tag;

      private:
        BaseIter iter;

      public:
        MapIterator(const BaseIter& iter)
            : iter(iter) {}
        MapIterator(const Self& other)
            : iter(other.iter) {}
        operator const_iterator() const {
            return const_iterator(iter);
        }

        NodeRefType operator*() {
            return *reinterpret_cast<NodeType*>(iter->pr);
        }

        const std::remove_reference_t<NodeRefType>& operator*() const {
            return *reinterpret_cast<NodeType*>(iter->pr);
        }

        std::remove_reference_t<NodeRefType>* operator->() {
            return reinterpret_cast<NodeType*>(iter->pr);
        }

        const std::remove_reference_t<NodeRefType>* operator->() const {
            return reinterpret_cast<NodeType*>(iter->pr);
        }

        Self& operator=(const Self& other) {
            Self tmp = other;
            swap(tmp);
            return *this;
        }

        void swap(Self& other) {
            std::swap(iter, other.iter);
        }

        Self& operator++() {
            ++iter;
            return *this;
        }

        Self operator++(int) {
            Self tmp = *this;
            ++*this;
            return tmp;
        }

        Self& operator--() {
            --iter;
            return *this;
        }

        Self operator--(int) {
            Self tmp = *this;
            --*this;
            return tmp;
        }

        template <typename OtherRefType, typename OtherListIterRefType>
        bool operator==(
            const MapIterator<OtherRefType, OtherListIterRefType>& other) const {
            return iter == other.iter;
        }

      private:
        friend class UnorderedMap;
        hash_type hash() const {
            return iter->hash_;
        }
        bool operator==(const typename MapNodeList::const_iterator& other) {
            return iter == other;
        }
    };

    using list_iterator = typename MapNodeList::iterator;
    using const_list_iterator = typename MapNodeList::const_iterator;

    constexpr static const Hash hash = Hash();
    MapNodeAlloc allocator;
    MapNodeList nodes;
    std::vector<list_iterator> buckets;
    double max_load_factor_ = 0.8;

  public:
    UnorderedMap(Alloc allocator = Alloc())
        : allocator(allocator), nodes(allocator), buckets(1, nodes.end(), allocator) {}
    UnorderedMap(const UnorderedMap& other)
        : allocator(AllocTraits::select_on_container_copy_construction(
              other.allocator)),
          nodes(allocator),
          buckets(1, nodes.end(), allocator) {
        for (const auto& i : other) {
            insert(i);
        }
    }
    UnorderedMap(UnorderedMap&& other)
        : allocator(std::move(other.allocator)), nodes(std::move(other.nodes)), buckets(std::move(other.buckets)) {
        other.buckets.resize(1, other.nodes.end());
    }

    ~UnorderedMap() {
        while (!empty()) {
            erase(begin());
        }
    }

  private:
    template <typename T>
    static hash_type get_hash(const std::pair<const Key, T>& pr) {
        return hash(pr->first);
    }

    template <typename T>
    static hash_type get_hash(const std::pair<Key, T>& pr) {
        return hash(pr->first);
    }

    template <typename T, typename... Args>
    static hash_type get_hash(const T& key, const Args&... /*unused*/) {
        return hash(key);
    }

    template <typename T, typename... Args>
    list_iterator find_(const T& key, const Args&... /*unused*/) {
        size_type hash = get_hash(key) % buckets.size();
        auto iter = buckets[hash];
        for (; iter != nodes.end() && iter->hash_ % buckets.size() == hash;
             ++iter) {
            if (Equal()(key, iter->pr->first)) {
                return iter;
            }
        }
        return nodes.end();
    }

    template <typename T, typename... Args>
    const_list_iterator find_(const T& key, const Args&... /*unused*/) const {
        size_type hash = get_hash(key) % buckets.size();
        auto iter = buckets[hash];
        for (; iter != nodes.end() && iter->hash_ % buckets.size() == hash;
             ++iter) {
            if (Equal()(key, iter->pr->first)) {
                return iter;
            }
        }
        return nodes.end();
    }

    template <bool Except, typename KeyType>
    list_iterator get_(KeyType&& key) {
        auto iter = find_(key);
        if (iter == nodes.end()) {
            if constexpr (Except) {
                throw std::out_of_range("No matching key.");
            } else {

                return emplace(std::forward<KeyType>(key)).first.iter;
            }
        }
        return iter;
    }

    template <typename KeyType>
    const_list_iterator get_(KeyType&& key) const {
        auto iter = find_(key);
        if (iter == nodes.end()) {
            throw std::out_of_range("No matching key.");
        }
        return iter;
    }

  public:
    Value& operator[](const Key& key) {
        return get_<false>(key)->pr->second;
    }

    Value& operator[](Key&& key) {
        Value& ref = get_<false>(std::move(key))->pr->second;
        return ref;
    }

    Value& at(const Key& key) {
        return get_<true>(key)->pr->second;
    }

    const Value& at(const Key& key) const {
        return get_(key)->pr->second;
    }

    Value& at(Key&& key) {
        return get_<true>(std::move(key))->pr->second;
    }

    const Value& at(Key&& key) const {
        return get_(std::move(key))->pr->second;
    }

    size_type size() const {
        return nodes.size();
    }
    bool empty() const {
        return nodes.size() == 0;
    }

    iterator begin() {
        return iterator(nodes.begin());
    }
    const_iterator begin() const {
        return cbegin();
    }
    const_iterator cbegin() const {
        return const_iterator(nodes.begin());
    }
    iterator end() {
        return iterator(nodes.end());
    }
    const_iterator end() const {
        return cend();
    }
    const_iterator cend() const {
        return const_iterator(nodes.end());
    }

    template <typename... Args>
    std::pair<iterator, bool> emplace(Args&&... args) {
        if ((nodes.size() + 1) * 1.0 / buckets.size() > max_load_factor_) {
            buckets.resize(2 * buckets.size(), nodes.end());
            rehash();
        }

        std::tuple<const Args&...> debug{args...};
        MapNode* node = MapNodeAllocTraits::allocate(allocator, 1);
        MapNodeAllocTraits::construct(allocator, node, Alloc(allocator),
                                      std::forward<Args>(args)...);
        hash_type hash = get_hash(node->pr->first) % buckets.size();
        auto iter = find_(node->pr->first);
        bool found = iter != nodes.end();
        if (!found) {
            nodes.emplace(buckets[hash], Alloc(allocator), std::move(*node));
            buckets[hash]--;
            iter = buckets[hash];
        }
        MapNodeAllocTraits::destroy(allocator, node);
        MapNodeAllocTraits::deallocate(allocator, node, 1);
        return std::make_pair(iterator(iter), !found);
    }

    std::pair<iterator, bool> insert(NodeType&& node) {

        return emplace(std::move(node));
    }

    std::pair<iterator, bool> insert(const NodeType& node) {

        return emplace(node);
    }

    template <typename P>
    std::pair<iterator, bool> insert(P&& value) {

        return emplace(std::forward<P>(value));
    }

    template <typename Iter>
    void insert(Iter start, Iter finish) {
        for (; start != finish; start++) {

            emplace(*start);
        }
    }

    void erase(const_iterator it) {
        hash_type hash = it.hash() % buckets.size();
        if (hash == buckets[hash]->hash_ % buckets.size()) {
            ++buckets[hash];
            if (buckets[hash] != nodes.end() && buckets[hash]->hash_ % buckets.size() != hash) {
                buckets[hash] = nodes.end();
            }
        }
        nodes.erase(it.iter);
    }

    void erase(const_iterator start, const_iterator end) {
        while (start != end) {
            erase(start++);
        }
    }

    iterator find(const Key& key) {
        return find_(key);
    }

    const_iterator find(const Key& key) const {
        return find_(key);
    }

    iterator find(Key&& key) {
        return find_(std::move(key));
    }

    const_iterator find(Key&& key) const {
        return find_(std::move(key));
    }

    void reserve(size_type n) {
        n = n / max_load_factor_ + 0.5;
        buckets.resize(n, nodes.end());
        rehash();
    }

    double load_factor() const {
        return 1.0 * nodes.size() / buckets.size();
    }

    double max_load_factor() const {
        return max_load_factor_;
    }

    void max_load_factor(double factor) {
        max_load_factor_ = factor;
        rehash();
    }

    void swap(UnorderedMap& other) {
        if (this == &other) {
            return;
        }
        std::swap(nodes, other.nodes);
        std::swap(buckets, other.buckets);
        if constexpr (std::is_base_of_v<
                          std::true_type,
                          typename AllocTraits::propagate_on_container_swap>) {
            std::swap(allocator, other.allocator);
        }
    }

    UnorderedMap& operator=(const UnorderedMap& other) {
        if (this == &other) {
            return *this;
        }
        while (!empty()) {
            erase(begin());
        }
        if constexpr (std::is_base_of_v<
                          std::true_type,
                          typename AllocTraits::
                              propagate_on_container_copy_assignment>) {
            allocator = other.allocator;
        }
        for (const auto& i : other) {

            emplace(i);
        }
        return *this;
    }

    UnorderedMap& operator=(UnorderedMap&& other) {
        if (this == &other) {
            return *this;
        }
        while (!empty()) {
            erase(begin());
        }
        if constexpr (std::is_base_of_v<
                          std::true_type,
                          typename AllocTraits::
                              propagate_on_container_move_assignment>) {
            allocator = std::move(other.allocator);
        }
        buckets = std::move(other.buckets);
        nodes = std::move(other.nodes);
        other.buckets.resize(1, other.nodes.end());

        return *this;
    }

    void rehash() {
        MapNodeList new_list;
        new_list.swap(nodes);
        std::fill(buckets.begin(), buckets.end(), nodes.end());
        for (auto&& i : new_list) {
            emplace(std::move(i));
        }
    }
};