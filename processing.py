
# Read defintion_df

pd.read_excel("/content/ISCO task list augmented.xlsx") 

definition_df["task_augmented"] = definition_df.apply(augment_title, axis=1)

# Embedding with TF-IDF

vectorizer = TfidfVectorizer(max_features=5000)
X_tfidf = vectorizer.fit_transform(definition_df["task_augmented"])

# Dimensionality Reduction (SVD/PCA)

svd = TruncatedSVD(n_components=50, random_state=42)
X_reduced = svd.fit_transform(X_tfidf)

# Clustering with KNN

k = 300
kmeans = KMeans(n_clusters=k, random_state=42, n_init=10, max_iter=300)
definition_df["cluster_id"] = kmeans.fit_predict(X_reduced)

# Output

definition_df[["isco_08_code", "task", "cluster_id"]].to_csv("Output/clustered_tasks.csv", index=False)
